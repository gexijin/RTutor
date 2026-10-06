#!/usr/bin/env bash
set -euo pipefail
# relocate_install_names_mac.sh - sever a bundled R.framework from the host.
#
# Mach-O bakes the full path of each dependency into the binary
# (LC_LOAD_DYLIB), so CRAN's binaries ask dyld for
#   /Library/Frameworks/R.framework/Versions/<ver>/Resources/lib/libR.dylib
# dyld tries that path FIRST and only consults DYLD_FALLBACK_LIBRARY_PATH once
# it fails. So on a Mac with R installed the app quietly loads the HOST's libR,
# and on a Mac without one it depends entirely on the fallback surviving
# hardened runtime. Rewriting to @loader_path removes both.
#
# @loader_path rather than @rpath: no extra LC_RPATH load command is needed, and
# the replacement is always SHORTER than the absolute path, so install_name_tool
# has no header-padding failure mode.
#
# RUN THIS AFTER THE PACKAGE LIBRARY IS INSTALLED. CRAN's macOS package binaries
# link libR.dylib by absolute path too, so a pass over base R alone leaves every
# package .so still pointing at the host.
#
# install_name_tool invalidates the signature it found, and on Apple Silicon an
# invalidly-signed Mach-O will not load. It re-signs ad-hoc itself, which is why
# the long-standing libomp step has never needed help - the preflight dlopens
# the .so files it patches (QUBIC, runibic) on every build. We rely on that here
# too. If it ever proves untrue the preflight dies loudly at 'Test bundled R
# runtime', which runs this staged tree before electron-builder signs anything.
#
# Usage:  relocate_install_names_mac.sh <R_HOME_DIR>

dst="${1:?Usage: relocate_install_names_mac.sh <R_HOME_DIR>}"
dst="$(cd "${dst}" && pwd -P)"

# CRAN's baked-in prefix. Deliberately stops at R.framework/: references come
# in three shapes and a narrower prefix would silently skip two of them, which
# would ALSO make the verification pass below print a false all-clear.
#   Versions/4.5-arm64/Resources/lib/libR.dylib   the usual form
#   Resources/lib/libR.dylib                      via the Current symlink
#   Versions/4.5-arm64/R                          -framework R, no /Resources/
framework_ref="/Library/Frameworks/R.framework/"

# Relative path from a directory inside ${dst} back up to ${dst}. Pure shell:
# realpath --relative-to is GNU-only and python is not a given on a build Mac.
up_to_root() {
  local dir="${1#"${dst}"}"
  dir="${dir#/}"
  local up="."
  if [ -n "${dir}" ]; then
    up=""
    local IFS='/'
    local part
    for part in ${dir}; do up="${up}../"; done
    up="${up%/}"
  fi
  printf '%s' "${up}"
}

find_machos() {
  # bin/* catches Rscript, which has no extension. Non-Mach-O files there
  # (bin/R is a shell script) just produce no otool output and fall through.
  find "${dst}" -type f \( -name '*.dylib' -o -name '*.so' -o -path '*/bin/*' \)
}

rewritten=0
missing=""
while IFS= read -r macho; do
  changed=0
  up="$(up_to_root "$(dirname "${macho}")")"
  chmod u+w "${macho}"

  # LC_ID_DYLIB. otool -D prints only a header line for non-dylibs, so an empty
  # line 2 falls through the case below.
  id_ref="$(otool -D "${macho}" 2>/dev/null | awk 'NR==2{$1=$1;print}')"
  case "${id_ref}" in
    "${framework_ref}"*)
      install_name_tool -id "@loader_path/${id_ref##*/}" "${macho}"
      changed=1
      ;;
  esac

  # LC_LOAD_DYLIB. The suffix after /Resources/ is the path within the
  # framework, e.g. "lib/libR.dylib", so it appends cleanly to the climb.
  while IFS= read -r dep; do
    case "${dep}" in
      "${framework_ref}"*)
        rest="${dep#*/R.framework/}"
        case "${rest}" in
          Resources/*)   suffix="${rest#Resources/}" ;;
          */Resources/*) suffix="${rest#*/Resources/}" ;;
          # The framework binary itself (Versions/<ver>/R or just R). It is a
          # symlink to Resources/lib/libR.dylib, which is what we ship.
          *)             suffix="lib/libR.dylib" ;;
        esac
        # Never point at something we do not ship: that would turn "loads the
        # host's copy" into "fails to load at all", which is worse. Report it
        # instead so the missing library gets bundled.
        if [ ! -f "${dst}/${suffix}" ]; then
          missing="${missing}
  ${macho#"${dst}"/} -> ${suffix}"
          continue
        fi
        install_name_tool -change "${dep}" "@loader_path/${up}/${suffix}" "${macho}"
        changed=1
        ;;
      # Anything absolute whose leaf name we already ship in lib/. This is how
      # the Fortran packages (DESeq2, edgeR, impute, preprocessCore) get fixed:
      # they link /opt/gfortran/..., the toolchain setup-r installs to build
      # them, which exists on no user's Mac. CRAN already ships libgfortran.5
      # and libquadmath.0 in Resources/lib, so we just repoint at our copy.
      # Scoped to lib/ deliberately - a leaf-name match anywhere would be a
      # licence to silently swap unrelated libraries.
      /usr/lib/*|/System/*|@*) ;;
      /*)
        leaf="${dep##*/}"
        if [ -f "${dst}/lib/${leaf}" ]; then
          install_name_tool -change "${dep}" "@loader_path/${up}/lib/${leaf}" "${macho}"
          changed=1
        fi
        ;;
    esac
  done < <(otool -L "${macho}" 2>/dev/null | awk '/^[[:space:]]/ {print $1}')

  [ "${changed}" -eq 1 ] && rewritten=$((rewritten + 1))
done < <(find_machos)

echo "Rewrote install names in ${rewritten} Mach-O file(s)"

if [ -n "${missing}" ]; then
  echo "ERROR: these reference framework libraries the bundle does not ship:${missing}" >&2
  echo "ERROR: bundle the library into ${dst}/lib before relocating." >&2
  exit 1
fi

# Fail loudly rather than shipping a half-relocated framework. A partial rewrite
# is the worst outcome: it works on any machine that has R and breaks only on
# the machines we cannot test on.
remaining=0
while IFS= read -r macho; do
  if otool -L "${macho}" 2>/dev/null | tail -n +2 | grep -q "${framework_ref}"; then
    echo "ERROR: still references the host framework: ${macho}" >&2
    otool -L "${macho}" | grep "${framework_ref}" >&2
    remaining=$((remaining + 1))
  fi
done < <(find_machos)

if [ "${remaining}" -gt 0 ]; then
  echo "ERROR: ${remaining} Mach-O file(s) still point at /Library/Frameworks" >&2
  exit 1
fi
echo "Verified: no Mach-O under ${dst} references the host R"
