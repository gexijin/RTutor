#!/usr/bin/env bash
set -euo pipefail
# verify_no_host_deps_mac.sh - fail if any Mach-O names a library that exists on
# the build machine but not on a user's Mac.
#
# On macOS every R package compiles from source: install_packages.R uses PPM's
# __linux__ binary path only on Linux, so the mac build gets the plain snapshot
# URL and builds against whatever Homebrew and Xcode provide. Anything a package
# links then carries that absolute path in its LC_LOAD_DYLIB. libomp is handled
# explicitly; nothing else is, and Bioconductor source builds routinely pull
# hdf5, gsl, libpng, jpeg, openssl and cairo.
#
# Allowed: @rpath / @loader_path / @executable_path, /usr/lib, /System, plus
# four named CRAN files that ship dangling /opt refs (see the case below).
# Every other absolute path is a reference that dangles on a user's machine.
#
# Run this on the PACKAGED app - it is the only check downstream of the prune
# and strip steps. Worth also running against the staging tree, which fails
# ~40 minutes earlier with the tree still on disk to diagnose from.
#
# Caveat: an @rpath/... reference is accepted without checking that some
# LC_RPATH actually resolves it. A green sweep means "no host paths", not
# "every reference resolves".
#
# Usage:  verify_no_host_deps_mac.sh <root>

root="${1:?Usage: verify_no_host_deps_mac.sh <root>}"
root="$(cd "${root}" && pwd -P)"

bad=0
while IFS= read -r macho; do
  rel="${macho#"${root}"/}"
  while IFS= read -r dep; do
    case "${dep}" in
      @*|/usr/lib/*|/System/*) continue ;;
    esac

    # CRAN's own R for macOS ships these same dangling references, and they
    # belong to components loaded lazily, only on use: tcltk, the X11 device
    # (modules/R_X11.so, modules/R_de.so) and the cairo device
    # (grDevices/libs/cairo.so). None is touched at startup, and stock CRAN R
    # is equally broken without XQuartz.
    #
    # Scoped to these four files on purpose, NOT allowed bundle-wide.
    # /opt/R/arm64 is CRAN's recipes prefix - hdf5, gsl, libpng, jpeg,
    # freetype, fontconfig - and every mac package compiles from source
    # against it, so a blanket /opt/R/* pass would silently swallow exactly
    # the references this sweep exists to catch. Anything new pointing at
    # /opt gets flagged and has to be added here deliberately.
    case "${rel}:${dep}" in
      */library/tcltk/libs/tcltk.so:/opt/R/*)       continue ;;
      */library/tcltk/libs/tcltk.so:/opt/X11/*)     continue ;;
      */library/grDevices/libs/cairo.so:/opt/X11/*) continue ;;
      */modules/R_X11.so:/opt/X11/*)                continue ;;
      */modules/R_de.so:/opt/X11/*)                 continue ;;
    esac

    case "${dep}" in
      /*)
        echo "  ${rel}"
        echo "      -> ${dep}"
        bad=$((bad + 1))
        ;;
    esac
  done < <(otool -L "${macho}" 2>/dev/null | awk '/^[[:space:]]/ {print $1}')
# pandoc is a shipped executable with no extension, not under a bin/ dir. Any
# future loose binary needs adding here too - the point of this sweep is not
# having unscanned Mach-Os.
done < <(find "${root}" -type f \
  \( -name '*.dylib' -o -name '*.so' -o -path '*/Resources/bin/*' -o -name 'pandoc' \))

if [ "${bad}" -gt 0 ]; then
  echo "ERROR: ${bad} dependency reference(s) point outside the bundle." >&2
  echo "ERROR: each names a path that exists here and not on a user's Mac." >&2
  echo "ERROR: copy the library into R.framework/Resources/lib and rewrite the" >&2
  echo "ERROR: reference with install_name_tool (see relocate_install_names_mac.sh)." >&2
  exit 1
fi
echo "Verified: every Mach-O dependency is bundle-relative or a system library"
