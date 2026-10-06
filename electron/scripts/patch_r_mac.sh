#!/usr/bin/env bash
set -euo pipefail
# patch_r_mac.sh — Make a relocated macOS R.framework portable.
#
# Apple's R.framework bakes R_HOME_DIR into bin/R as an absolute path:
#   /Library/Frameworks/R.framework/Resources
# After electron-builder packages the app into a .app bundle, that path
# no longer exists.  Rewrite the top-of-file variables in bin/R so they
# are computed from the script's own location.
#
# bin/Rscript is a separate compiled binary with its own baked-in path, so
# main.js sets RHOME (no underscore) at spawn time instead. R_HOME alone
# does nothing for it.
#
# Severing the framework's remaining ties to the host is a separate step:
# see relocate_install_names_mac.sh, which must run AFTER the package
# library is installed.
#
# Usage:  patch_r_mac.sh <R_HOME_DIR>
#   e.g.  patch_r_mac.sh electron/runtime/R.framework/Resources

dst="${1:?Usage: patch_r_mac.sh <R_HOME_DIR>}"

if [ ! -f "${dst}/bin/R" ]; then
  echo "ERROR: ${dst}/bin/R not found — is this an R home directory?" >&2
  exit 1
fi

R_SCRIPT="${dst}/bin/R"

# BSD sed on macOS needs -i '' for in-place, but using a temp-file pattern
# keeps this portable and avoids the empty-arg footgun.
patch_line() {
  local pattern="$1"
  local replacement="$2"
  local file="$3"
  sed "s|${pattern}|${replacement}|" "${file}" > "${file}.new"
  mv "${file}.new" "${file}"
}

# bin/R lives at <R_HOME>/bin/R, so dirname/.. = R_HOME.
# Using `cd ... && pwd -P` avoids GNU-only readlink -f.
patch_line '^R_HOME_DIR=.*'    'R_HOME_DIR="$(cd "$(dirname "$0")/.." \&\& pwd -P)"' "${R_SCRIPT}"
patch_line '^R_SHARE_DIR=.*'   'R_SHARE_DIR=${R_HOME_DIR}/share'                     "${R_SCRIPT}"
patch_line '^R_INCLUDE_DIR=.*' 'R_INCLUDE_DIR=${R_HOME_DIR}/include'                 "${R_SCRIPT}"
patch_line '^R_DOC_DIR=.*'     'R_DOC_DIR=${R_HOME_DIR}/doc'                         "${R_SCRIPT}"

chmod +x "${R_SCRIPT}"
echo "Patched bin/R path variables to be self-referencing"
