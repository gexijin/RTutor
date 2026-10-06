# electron/scripts/get_r_windows.ps1
$ErrorActionPreference = "Stop"

# ==================== Logging ====================
# All output goes to both terminal and logfile
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Definition
$timestamp = Get-Date -Format "yyyyMMdd_HHmmss"
$logFile = Join-Path $scriptDir "get_r_windows_${timestamp}.log"

# Start a transcript so every Write-Host / stdout / stderr line is captured
Start-Transcript -Path $logFile -Append
Write-Host "Logging to: $logFile"

# -------- Config --------
$Rver = $env:R_VERSION
if (-not $Rver -or $Rver -eq "") { $Rver = "4.5.3" }   # default version

Write-Host "Using R version: $Rver"

# runtime destination — matches the production layout in main.js getRuntime()
# (path.join(rp, 'runtime', 'R.win')) and the Windows CI workflow's robocopy target.
# Resolve paths relative to the script's own location, not cwd, so the script
# works no matter where it's invoked from.
$electronDir = Split-Path -Parent $scriptDir
$destR       = Join-Path $electronDir "runtime\R.win"
New-Item -ItemType Directory -Force -Path $destR | Out-Null

# Temp working dir
$tmp = Join-Path $env:TEMP ("rwin_" + [Guid]::NewGuid())
New-Item -ItemType Directory -Force -Path $tmp | Out-Null

try {
  # -------- Download installer --------
  $exe = Join-Path $tmp "R-$Rver-win.exe"
  $urls = @(
    "https://cloud.r-project.org/bin/windows/base/old/$Rver/R-$Rver-win.exe",
    "https://cran.r-project.org/bin/windows/base/old/$Rver/R-$Rver-win.exe",
    "https://cloud.r-project.org/bin/windows/base/R-$Rver-win.exe",
    "https://cran.r-project.org/bin/windows/base/R-$Rver-win.exe"
  )

  $downloaded = $false
  foreach ($u in $urls) {
    Write-Host "Trying $u ..."
    try {
      Invoke-WebRequest -Uri $u -OutFile $exe -UseBasicParsing
      if ((Get-Item $exe).Length -gt 0) { $downloaded = $true; Write-Host "Downloaded $u"; break }
    } catch { Write-Host "Download failed from $u, trying next mirror..." }
  }
  if (-not $downloaded) { throw "Failed to download R-$Rver Windows installer from all candidates." }

  # -------- Install silently into temp base (installer creates R-x.y.z under here) --------
  $installBase = Join-Path $tmp "R-install"
  New-Item -ItemType Directory -Force -Path $installBase | Out-Null

  Write-Host "Installing R silently into $installBase ..."
  & $exe /VERYSILENT /DIR="$installBase" /NORESTART /SP- /SUPPRESSMSGBOXES | Out-Null

  # -------- Locate the versioned R home (R-x.y.z) --------
  # Prefer by discovering Rscript.exe, then take its parent twice to get R-x.y.z
  $rscript = Get-ChildItem -Recurse -Path $installBase -Filter Rscript.exe -File -ErrorAction SilentlyContinue | Select-Object -First 1
  if (-not $rscript) { throw "Rscript.exe not found under $installBase after install." }

  $rHome = $rscript.Directory.Parent  # ...\R-x.y.z
  if (-not (Test-Path (Join-Path $rHome.FullName "bin\R.exe"))) {
    throw "R.exe not found in $(Join-Path $rHome.FullName 'bin'); unexpected layout."
  }

  Write-Host "Detected R home: $($rHome.FullName)"

  # -------- Normalize to flat layout: ../runtime/R.win/{bin,library,...} --------
  if (Test-Path $destR) {
    Write-Host "Cleaning existing $destR ..."
    Remove-Item -Recurse -Force $destR
  }
  New-Item -ItemType Directory -Force -Path $destR | Out-Null

  # Copy CONTENTS of R-x.y.z into ../runtime/R.win (so we get R.win/bin, not R.win/R-x.y.z/bin)
  Write-Host "Copying portable R to $destR ..."
  Copy-Item -Recurse -Force -Path (Join-Path $rHome.FullName "*") -Destination $destR

  # -------- Sanity checks --------
  $destRscript = Join-Path $destR "bin\Rscript.exe"
  if (-not (Test-Path $destRscript)) { throw "Missing $destRscript after copy." }
  $destRexe = Join-Path $destR "bin\R.exe"
  if (-not (Test-Path $destRexe)) { throw "Missing $destRexe after copy." }
  $destRdll = Join-Path $destR "bin\x64\R.dll"
  if (-not (Test-Path $destRdll)) { Write-Host "Warning: $destRdll not found (some builds place R.dll under bin only)"; }

  Write-Host "Rscript located at: $destRscript"
  & $destRscript --version 2>&1 | Write-Host

  Write-Host "✅ Windows R runtime ready under $destR"

  # ==================== Pandoc (needed by rmarkdown for HTML reports) ====================
  # rmarkdown shells out to the pandoc binary; it's not an R package, so
  # install.packages() can't fetch it. Ship the official portable build
  # alongside R. See main.js getRuntime() for the matching PATH/RSTUDIO_PANDOC
  # wiring.
  $pandocVer = $env:PANDOC_VERSION
  if (-not $pandocVer -or $pandocVer -eq "") { $pandocVer = "3.11" }
  $destPandoc = Join-Path $electronDir "runtime\pandoc.win"
  $pandocZip = Join-Path $tmp "pandoc.zip"
  Invoke-WebRequest -Uri "https://github.com/jgm/pandoc/releases/download/$pandocVer/pandoc-$pandocVer-windows-x86_64.zip" -OutFile $pandocZip -UseBasicParsing
  Expand-Archive -Path $pandocZip -DestinationPath $tmp -Force
  New-Item -ItemType Directory -Force -Path $destPandoc | Out-Null
  Copy-Item -Force -Path (Join-Path $tmp "pandoc-$pandocVer\pandoc.exe") -Destination (Join-Path $destPandoc "pandoc.exe")
  Write-Host "✅ pandoc $pandocVer ready under $destPandoc"

  # ==================== Install R packages + RTutor ====================
  # The same package set the CI build bundles (install_packages.R), then RTutor itself
  # from this repo. Slow: several hundred packages.
  $lib      = Join-Path $destR "library"
  $libR     = $lib -replace '\\', '/'
  $Rexe     = Join-Path $destR "bin\R.exe"
  $repoRoot = Split-Path -Parent $electronDir
  Write-Host "Library : $lib"

  $savedPref = $ErrorActionPreference
  $ErrorActionPreference = "Continue"

  # Suppress the developer's user library (default %LOCALAPPDATA%\R\win-library\4.5)
  # so installs don't skip transitives it considers "already installed"
  # there — which would leave the bundled runtime missing rlang/cli/glue/etc. at
  # app launch. R treats the literal string "NULL" as "no user library".
  # --vanilla alone is not enough on Windows because R sets the default
  # R_LIBS_USER path even when .Renviron is suppressed.
  $savedRLibsUser = $env:R_LIBS_USER
  $env:R_LIBS_USER = "NULL"
  try {
    & $destRscript --vanilla (Join-Path $scriptDir "install_packages.R") $libR 2>&1 | Write-Host
    if ($LASTEXITCODE -ne 0) { throw "install_packages.R failed with exit code $LASTEXITCODE" }

    & $Rexe --vanilla CMD INSTALL --library="$lib" "$repoRoot" 2>&1 | Write-Host
    if ($LASTEXITCODE -ne 0) { throw "Installing RTutor failed with exit code $LASTEXITCODE" }

    # Sanity-check the pandoc bundled above: RSTUDIO_PANDOC is the same env
    # var main.js sets on the spawned R process, so this is the same lookup
    # path the packaged app uses.
    $env:RSTUDIO_PANDOC = $destPandoc
    & $destRscript --vanilla -e "cat('pandoc_available:', rmarkdown::pandoc_available(), '| version:', as.character(rmarkdown::pandoc_version()), '\n')" 2>&1 | Write-Host
    Remove-Item Env:\RSTUDIO_PANDOC
  } finally {
    $env:R_LIBS_USER = $savedRLibsUser
  }

  $ErrorActionPreference = $savedPref

  Write-Host ""
  Write-Host "✅ R packages and RTutor installed. Run the app with: cd electron; npm start"
}
finally {
  if (Test-Path $tmp) {
    Write-Host "Cleaning up temp dir $tmp ..."
    Remove-Item -Recurse -Force $tmp
  }
  Stop-Transcript
}
