#!/usr/bin/env bash
set -euo pipefail

# ==================== Config ====================
VER="${R_VERSION:-4.5.3}"   # override with env R_VERSION=...
# Auto-detect ARCH unless overridden via R_ARCH
if [[ -z "${R_ARCH:-}" ]]; then
  case "$(uname -m)" in
    arm64|aarch64) ARCH="arm64" ;;
    x86_64|amd64)  ARCH="x86_64" ;;
    *)             ARCH="x86_64" ;;
  esac
else
  ARCH="${R_ARCH}"
fi

# Candidate URLs (primary + fallbacks)
CANDIDATES=(
  "https://cran.r-project.org/bin/macosx/big-sur-${ARCH}/base/R-${VER}-${ARCH}.pkg"
  "https://cloud.r-project.org/bin/macosx/big-sur-${ARCH}/base/R-${VER}-${ARCH}.pkg"
  # Intel builds are sometimes published without the -x86_64 suffix
  "https://cran.r-project.org/bin/macosx/big-sur-x86_64/base/R-${VER}.pkg"
  "https://cloud.r-project.org/bin/macosx/big-sur-x86_64/base/R-${VER}.pkg"
)

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
ELECTRON_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Stage R.framework at the production layout — matches main.js getRuntime()
# (path.join(rp, 'runtime', 'R.framework')) and build-desktop.yml.
RFRAMEWORK_DEST="${ELECTRON_DIR}/runtime/R.framework"

TMP="$(mktemp -d)"
cleanup(){ rm -rf "$TMP"; }
trap cleanup EXIT

PKG_PATH=""
for URL in "${CANDIDATES[@]}"; do
  echo "Trying ${URL} ..."
  if curl -fL --retry 3 --connect-timeout 20 -o "${TMP}/R.pkg" "${URL}"; then
    PKG_PATH="${TMP}/R.pkg"
    echo "Downloaded: ${URL}"
    break
  fi
done

if [[ -z "${PKG_PATH}" ]]; then
  echo "ERROR: Unable to download R ${VER} pkg for macOS (tried ${#CANDIDATES[@]} URLs)." >&2
  exit 1
fi

echo "Expanding pkg ..."
pkgutil --expand-full "${PKG_PATH}" "${TMP}/expanded"

# Locate R.framework inside expanded pkg
RFW="$(/usr/bin/find "${TMP}/expanded" -type d -name 'R.framework' -print -quit || true)"
if [[ -z "${RFW}" ]]; then
  echo "ERROR: R.framework not found in expanded package:" >&2
  /usr/bin/find "${TMP}/expanded" -maxdepth 4 -print
  exit 1
fi

echo "Copying R.framework to ${RFRAMEWORK_DEST} ..."
rm -rf "${RFRAMEWORK_DEST}"
mkdir -p "$(dirname "${RFRAMEWORK_DEST}")"
ditto "${RFW}" "${RFRAMEWORK_DEST}"

echo "Rscript version:"
"${RFRAMEWORK_DEST}/Resources/bin/Rscript" --version
echo "✅ macOS R runtime ready at: ${RFRAMEWORK_DEST}"

# ==================== Pandoc (needed by rmarkdown for HTML reports) ====================
# rmarkdown shells out to the pandoc binary; it's not an R package, so
# install.packages() can't fetch it. Ship the official portable build
# alongside R. See main.js getRuntime() for the matching PATH/RSTUDIO_PANDOC
# wiring. ARCH is the same arm64/x86_64 value used for the R download above.
PANDOC_VER="${PANDOC_VERSION:-3.11}"
PANDOC_DEST="${ELECTRON_DIR}/runtime/pandoc.mac"
curl -fL --retry 3 --connect-timeout 20 -o "${TMP}/pandoc.zip" \
  "https://github.com/jgm/pandoc/releases/download/${PANDOC_VER}/pandoc-${PANDOC_VER}-${ARCH}-macOS.zip"
ditto -x -k "${TMP}/pandoc.zip" "${TMP}/pandoc-extracted"
mkdir -p "${PANDOC_DEST}"
cp "${TMP}/pandoc-extracted/pandoc-${PANDOC_VER}-${ARCH}/bin/pandoc" "${PANDOC_DEST}/pandoc"
chmod +x "${PANDOC_DEST}/pandoc"
echo "✅ pandoc ${PANDOC_VER} ready at: ${PANDOC_DEST}/pandoc"

# ==================== Install R packages + RTutor ====================
# The same package set the CI build bundles (install_packages.R), then RTutor itself
# from this repo. Slow: several hundred packages.
RSCRIPT="${RFRAMEWORK_DEST}/Resources/bin/Rscript"
RBIN="${RFRAMEWORK_DEST}/Resources/bin/R"
LIB="${RFRAMEWORK_DEST}/Resources/library"
REPO_ROOT="$(cd "${ELECTRON_DIR}/.." && pwd)"
echo "Library : ${LIB}"

# Suppress the developer's user library (default ~/Library/R/x.y/library)
# so installs don't skip transitives it considers "already installed"
# there — which would leave the bundled runtime missing rlang/cli/glue/etc. at
# app launch. R treats the literal string "NULL" as "no user library".
# --vanilla alone is not enough because R sets the default R_LIBS_USER path
# even when .Renviron is suppressed.
R_LIBS_USER=NULL "${RSCRIPT}" --vanilla "${SCRIPT_DIR}/install_packages.R" "${LIB}"
R_LIBS_USER=NULL "${RBIN}" --vanilla CMD INSTALL --library="${LIB}" "${REPO_ROOT}"

# Sanity-check the pandoc bundled above: RSTUDIO_PANDOC is the same env var
# main.js sets on the spawned R process, so this is the same lookup path the
# packaged app uses.
RSTUDIO_PANDOC="${PANDOC_DEST}" R_LIBS_USER=NULL "${RSCRIPT}" --vanilla -e "cat('pandoc_available:', rmarkdown::pandoc_available(), '| version:', as.character(rmarkdown::pandoc_version()), '\n')"

echo "✅ R packages and RTutor installed. Run the app with: cd electron && npm start"
