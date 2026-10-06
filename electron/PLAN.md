# UIUC RTutor Desktop: Implementation Plan

This plan covers packaging the UIUC RTutor Shiny app (branch `uiuc_main`) as a downloadable desktop app for
macOS (Apple Silicon) and Windows (x64). It is adapted from an existing Electron + R desktop build, trimmed down to what RTutor needs.

Status: **implemented on `uiuc_electron` (2026-10-06); not yet built in CI.** Where the implementation differs from
the original plan, see "Changes made during implementation" at the end.

---

## 0. Decisions already made

| Topic | Decision |
|---|---|
| API key | One OpenAI `sk-` key is embedded in the installer. The GitHub secret `DOG_LOVER` on gexijin/RTutor holds it. The build writes it into the app as `OPENAI_API_KEY`, which is the only name the R code reads. The key has a $400 spending cap and expires at the end of the semester. A new key and a new build are made every semester. |
| Per-user limits | None. |
| Where installers are published | GitHub Releases on the **public** gexijin/RTutor repo. Anyone can download them and extract the key. The cap and the expiry date are what limit the damage. |
| Packages | **Option B:** a large, fixed package set is bundled at build time. The app never installs packages on a student's laptop. A missing package shows a clear message. |
| Package list | Built specifically for RTutor. There is no student usage data: `usage_data.db` exists neither on `rtutor8core` nor in `uiuc1`. The list therefore comes from the app's own dependencies, the curated list in `librarySetup.R`, and common stats-course packages. It is then checked against the code the model actually writes for the app's example prompts (§4). |
| pandoc | Bundled, so the HTML report and EDA report work. |
| Feedback form | Already unreachable: the "More" menu is hidden by CSS (`R/mod_01_styles.R:31`). No change. |
| Platforms | macOS Apple Silicon (arm64) first, then Windows x64. No Linux, no Intel Mac. |
| Code signing | None (no paid certificates). Mac builds are **ad-hoc signed**, which is free. electron-builder 26.8.2 supports this with `mac.identity: "-"` (verified in its schema). Without any signature, Apple Silicon reports "damaged" and offers no "Open Anyway". Students get a step-by-step guide for Gatekeeper and SmartScreen (§7). |
| Google Analytics | Kept (`ga.html`). |
| Expiry message | A 401 from OpenAI shows "this version has expired" with a download link. |
| Build triggers | Every push to `uiuc_electron` produces test installers (Actions artifacts, not published). Every `uiuc-desktop-v*` tag produces a GitHub Release. |
| Naming | Tags `uiuc-desktop-v1.0.0`. Releases are titled "UIUC RTutor Desktop 1.0.0" and are **not** marked Latest, so the web app's v0.98.3 release stays on top. |
| Branch | `uiuc_electron`, cut from `uiuc_main`, and merged back once tested. |
| Shinylive | `uiuc_shinylive` will be retired once this ships. Jenna will remove it. |
| Icon | The RTutor hex sticker, padded onto a transparent square. |
| Releases | Tag builds create a **draft**. Jenna tests it and publishes it by hand. |
| Release description | Lists the minimum systems (Apple Silicon Mac on macOS 12+, 64-bit Windows 10/11) and the install steps. |
| Installer too big | If it's over 1.9 GB, remove the least common packages from list (c) in §4, working up from the bottom, until it fits. |
| Paid LLM checks | The coverage check (§4) and `tests/eval_prompt_quality.R` use the class key. **Jenna runs them by hand later. They are never run as part of this work.** |
| Failing `mod_16_qa` tests | Left alone. They check for Q&A CSS classes that commit `7941446` (Q&A history redesign) moved out of `mod_16_qa.R`. The tests are out of date; the app isn't broken. Unrelated to Electron. |

---

## 1. Branch and repo setup

1. Create `uiuc_electron` from `uiuc_main` at `9cd2b24` or later.
2. `.gitignore` additions: `electron/.env`, `electron/node_modules/`, `electron/runtime/`, `electron/app/`,
   `electron/dist/`.
3. `.Rbuildignore` additions: `^electron$` and `^\.github$`. Without them, `R CMD build` would put the Electron
   tree, node_modules and the bundled runtime inside the RTutor package tarball.
4. `CLAUDE.md`: add a short "Desktop app" section covering where the files live, how a release is made,
   `RTUTOR_DESKTOP`, and the `DOG_LOVER` → `OPENAI_API_KEY` mapping.

---

## 2. R app changes (in `R/`, also harmless on the server)

### 2.1 Fix `DESCRIPTION` Imports
The build installs only what `DESCRIPTION` and the package list name. These are used by the app but missing from
Imports today:
`tidyverse` (attached in `mod_02_load_data.R:52`), `httr`, `jsonlite`, `shinyalert`, `lubridate`, `readxl`,
`commonmark`, `pacman`, `htmltools`, `rlang`, `knitr` and `rmarkdown` (both currently Suggests, but the reports
need them at runtime).

Remove `openai` from Imports. Checked: there are no `openai::` calls, no `library(openai)`, and nothing in
`NAMESPACE`. Every LLM call goes through `create_response()` (`fct_helpers.R`), which POSTs JSON directly to
`https://api.openai.com/v1/responses` with `httr` and an `Authorization: Bearer <OPENAI_API_KEY>` header. Also fix
the report template's credits text (`mod_09_report.R:124`), which still says the code was generated with the
`openai` package. `shinyWidgets` is not in Imports and is no longer used after the key change. No
action needed for it.

`reticulate` is only used when `use_python` is TRUE, and that setting is hard-wired to FALSE (`mod_11`). It is left
out of the bundle.

Add a test (`tests/testthat/test-desktop.R`) that every `pkg::` used in `R/` appears in DESCRIPTION Imports or
base R. Without it this gap reopens silently.

### 2.2 Desktop mode flag
`main.js` sets `RTUTOR_DESKTOP=1` when it starts R. In `fct_helpers.R`, next to `on_server`:
```r
on_desktop <- function() nzchar(Sys.getenv("RTUTOR_DESKTOP"))
```
- `clean_cmd()` (`fct_helpers.R:989`): today, when `on_server.txt` is missing, every `library(x)` is rewritten to
  `pacman::p_load(x)`, which installs from CRAN. On desktop that would try to write into the read-only app folder.
  Change the condition to `if (!on_server && !on_desktop)`.
- Missing-package message (`mod_07_run_code.R`, where `error_message` is set): if the error matches
  `there is no package called ‘x’`, add *"Package x isn't included in the UIUC RTutor desktop app. Try asking for
  the analysis without it."* The error-explanation tutor (`explain_error`) still runs as usual.
- Upload limit: desktop takes the existing non-server branch in `app_server.R:20` (10 GB). No change.

### 2.3 API error popup (`mod_06_error_hist.R:21`, `api_error_modal`)
Some of the current wording is stale since the key change: it tells students to "check your key in Settings", and
that option no longer exists.
- **401** (key rejected or expired): "This version of UIUC RTutor has expired. Download the latest version from
  <releases link>." The link is a real `<a target="_blank">`. The URL is a constant in `fct_helpers.R`
  (`desktop_download_url`). The plain releases page `https://github.com/gexijin/RTutor/releases` also lists the
  web app's old releases. Use `.../releases?q=uiuc-desktop&expanded=true` if that filter works (check during
  implementation), and otherwise the plain page with "look for *UIUC RTutor Desktop*".
- **429**: OpenAI returns this both for rate limits and for a used-up budget (`insufficient_quota`). If the message
  contains `quota`, show "The class's AI budget for this semester has been used up. Tell your instructor." Otherwise
  keep the "wait and retry" text.
- **403** and the default branch: drop the "Settings" and "your OpenAI account" wording. The default becomes "Could
  not reach the AI server. Check your internet connection."
- The Q&A tab uses the same popup (`mod_16_qa.R:114`), so one fix covers both. The prompt check and error
  explanations deliberately allow the request through on failure, so they need no change.

### 2.4 Tests
- `test-desktop.R`:
  - `clean_cmd()` keeps `library()` when `RTUTOR_DESKTOP=1` and rewrites to `p_load` when it isn't set.
  - The DESCRIPTION coverage check from §2.1.
  - The 401 popup contains the download link.
- The 3 `mod_16_qa` failures that already exist in `test-uiuc_improvements.R` are out of scope: the tests are out of date (see §0).

---

## 3. Electron shell (`electron/`)

### 3.1 Delete
Everything from the source build that RTutor doesn't need, including its account, licensing and code-signing pieces,
Linux scripts, and macOS entitlement files (only needed with the hardened runtime, which is off, see §3.6).

### 3.2 `main.js`: a small rewrite (about 400 lines)
**Keep** (each fixes a known desktop-R problem):
- the single-instance lock
- `getRuntime()` (Windows and macOS branches only)
- `isWritableDir`
- `safeKill` (taskkill /T /F on Windows)
- `waitForHttp` with the 30 s per-attempt abort
- `getFreePort`
- the splash and progress updates
- `loadAppURL` retries
- detecting the port from Shiny's "Listening on" message
- the "Server terminated" page
- the crash guards
- logging to a file
- killing R in `before-quit`
- the 30 s heartbeat

**Remove:** everything specific to the source app (accounts, licensing, its data downloads, package renaming,
and idle-disconnect diagnostics).

**Change:**
- Log file: `<temp>/uiuc-rtutor-electron.log`.
- Working folder: `app.getPath('userData')/rtutor`, always. Drop the "launch directory" option: students start
  the app from the Dock or Start menu, and an inherited folder only causes surprises.
- API key: read `electron/.env` with `dotenv.parse()`, **not** `dotenv.config()`. Pass `OPENAI_API_KEY` explicitly
  in the `spawn()` env so it overrides any `OPENAI_API_KEY` the student's machine already has. If `.env` is missing
  or the key is empty, show a dialog ("This build has no API key, please download the official release") and quit,
  instead of launching an app where every request fails.
- Windows `getRuntime()`: don't point `R_USER` at the read-only install folder. `R_USER` is what `~` means in R on
  Windows, so generated code that saves to `~/...` would fail. Point it at the writable data folder instead.
- spawn env:
  - `RTUTOR_DESKTOP=1` and `OPENAI_API_KEY`
  - `RTUTOR_HOST` / `RTUTOR_PORT` / `RTUTOR_DATA_DIR`
  - `R_LIBS_USER=<bundled library>`, `R_LIBS=''`, `R_LIBS_SITE=''`
  - plus `getRuntime()`'s `R_HOME`/`RHOME`/`DYLD_FALLBACK_LIBRARY_PATH`/`RSTUDIO_PANDOC`/`PATH`
- The key is never logged.
- Menu: File/Edit/View. Remove "Toggle Developer Tools" from packaged builds so students don't stumble into
  devtools. Keep reload and zoom.
- Update check: `checkForUpdates()` 5 s after load (§3.4).

### 3.3 `bootstrap.R`: rewrite to about 40 lines
Read host, port, lib and data dir from the environment. Set `.libPaths(lib)`. `setwd(data_dir)`, then sink output
to `data_dir/electron_r.log`. Set `options(shiny.launch.browser = FALSE, golem.app.prod = TRUE)`. Then
`shiny::runApp(RTutor::run_app(), host, port, launch.browser = FALSE)`, wrapped in tryCatch so the cause of any
fatal error lands in the log.

### 3.4 `updater.js`
`RELEASES_API` → `https://api.github.com/repos/gexijin/RTutor/releases?per_page=50`. Keep only non-draft releases
whose `tag_name` starts with `uiuc-desktop-v`, pick the highest version, and compare it with `app.getVersion()`.
The dialog wording becomes "UIUC RTutor Desktop X is available". Known limitation: the GitHub API allows 60
anonymous requests per hour per IP address, so many students behind one campus network address may make the check
fail quietly. That's acceptable, because the expiry popup is the real safety net.

### 3.5 `splash.html`
Rebrand to "UIUC RTutor", version from `app.getVersion()`, and the RTutor logo. Keep the progress bar and the
`{{LOG_FILE}}` hint.

### 3.6 `package.json`
- `name`: `uiuc-rtutor-desktop`. This also names the userData folder.
- `productName`: `UIUC RTutor`. `version`: `1.0.0`. `description`, `author`, `repository` →
  gexijin/RTutor. `license` taken from RTutor's DESCRIPTION.
- dependencies: `dotenv` only. devDependencies: `electron` 39.8.4, `electron-builder` ^26.8.2.
- Scripts: `start`, `dist`. Drop `dev`.
- `build.appId`: `ai.rtutor.uiuc.desktop`. `asar: false` (R needs real files).
- `files`: `main.js`, `updater.js`, `bootstrap.R`, `splash.html`, `.env`, `package.json`, `node_modules/**`.
- `extraResources`: `runtime` → `runtime`. `app` is no longer needed: there's no `app.R` on this branch, and
  `bootstrap.R` loads the installed `RTutor` package. The `resources` entry goes too.
- `mac`:
  - `target: dmg`, `arch: arm64`
  - `identity: "-"` (ad-hoc), `hardenedRuntime: false`, `gatekeeperAssess: false`, `notarize: false`
  - no entitlements, `icon: build/icon-mac.png`
  - `artifactName: UIUC-RTutor-${version}-mac-arm64.${ext}` (no spaces)
- `win`: `nsis` x64, `artifactName: UIUC-RTutor-${version}-win-x64.${ext}`.
- `nsis`:
  - `oneClick: true` and `perMachine: false` (the default): installs for the current user only, so no admin
    password is needed.
  - Start-menu and desktop shortcuts named "UIUC RTutor".
- Then run `npm install` to regenerate `package-lock.json`.

### 3.7 Other files
- ~~`.npmrc`~~: dropped, see §12.
- `.env.example`: one line, `OPENAI_API_KEY=sk-...`, plus a comment that CI writes `.env` from the `DOG_LOVER`
  secret.
- Icons: `build/icon.png` (256 px, Windows) and `build/icon-mac.png` (1024 px), made from
  `inst/app/www/hex_sticker_rtutor_black.png` (518×600) padded onto a transparent square.
- Dev scripts: keep `get_r_mac.sh` and `get_r_windows.ps1` for building locally. Make them install the full
  package set and RTutor from the repo root. Keep the `R_LIBS_USER=NULL` trick.
  `patch_r_mac.sh`, `relocate_install_names_mac.sh` and `verify_no_host_deps_mac.sh` stay as they are.

---

## 4. Package list (`electron/scripts/install_packages.R`)

Rewrite the script so it installs an **explicit list plus RTutor's DESCRIPTION dependencies**. Every dependency of
each listed package is pulled in automatically. No Bioconductor at all, so no BiocManager.

**Sources:**
- **(a) The app itself:** DESCRIPTION Imports after §2.1, installed with `pak::local_install_deps()`.
- **(b) The curated server list** (`~/RTutor_server/classes/librarySetup.R` on rtutor8core), cleaned up:
  - **Keep:** tidyverse, readxl, gridExtra, DataExplorer, ggfortify, corrplot, ggcorrplot, GGally, corrr, pheatmap,
    RColorBrewer, gplots, dendextend, ggalluvial, kernlab, cluster, fpc, mclust, dbscan, factoextra, ggdendro,
    NbClust, clusterGeneration, psych, qgraph, mgcv, nlme, caret, rpart, randomForest, e1071, nnet, gbm, ROCR, klaR,
    class, neuralnet, xgboost, forecast, tseries, TSA, xts, lubridate. `ggbiplot` too, if it's on CRAN at build time.
  - **Drop:**
    - `PCA` is not a package.
    - `PCAExplorer` is really Bioconductor's heavy `pcaExplorer`.
    - `pcaMethods` and `ComplexHeatmap` are Bioconductor (pheatmap covers heatmaps).
    - `d3heatmap` and `bootPCA` have been removed from CRAN.
    - `liblinear` is a wrong name (it's `LiblineaR`, not needed).
    - `mlr` is heavy and superseded.
    - `openai`, `shinyBS`, `remotes` are not needed.
- **(c) Common stats-course packages**, kept **in order from most to least commonly used**, so trimming for size
  (§0) removes from the bottom:
  - **Data:** data.table, janitor, skimr, haven, writexl, scales, reshape2, zoo
  - **Plots:** ggpubr, ggrepel, ggthemes, ggridges, patchwork, cowplot, viridis, ggbeeswarm, vcd, treemapify,
    plotly, hexbin
  - **Description and tables:** Hmisc, DescTools, moments, summarytools, tableone, gtsummary, kableExtra, broom
  - **Inference:** car, emmeans, rstatix, effectsize, nortest, coin, multcomp, pwr, lmtest, sandwich, boot, MASS
  - **Models:** lme4, lmerTest, glmnet, survival, survminer, ordinal
  - **ML:** ranger, rpart.plot, pROC
  - **Multivariate:** FactoMineR, Rtsne
  - Recommended packages (MASS, survival, nlme, mgcv, cluster, class, nnet, rpart, boot, lattice) already ship with
    R, but they are listed anyway so the coverage check counts them.

**How packages are installed:**
- **Windows:** prebuilt binaries from the Posit Package Manager **snapshot** for a fixed date, so every build gets
  the same versions.
- **macOS:** CRAN's own prebuilt arm64 binaries (`type = "binary"`) for the R version being bundled. Compiling every
  Mac package from source would need Fortran for many stats packages and is slow and fragile. CRAN's Mac
  binaries are not pinned to a date. If Posit Package Manager serves macOS binaries for this R version when we
  implement, use the snapshot there too.
- **R version:** the current R release when we implement. Any change must be made in both
  workflows and both `get_r_*` scripts.
- The build **fails** if any listed package doesn't install. A silently missing package is exactly what this plan
  is trying to prevent.

**Coverage check (new, run by hand, like `tests/eval_prompt_quality.R`):** `electron/scripts/check_package_coverage.R`
1. Loads the app with `devtools::load_all()`.
2. Sends each example prompt from `inst/app/www/uiuc_demo_questions.csv` and `demo_questions.csv`, about 220 in all,
   through the same prompt building and `create_response()` the app uses, with each prompt's dataset.
3. Extracts every package the generated code loads.
4. Prints any that aren't in the bundle.

This costs about $2 of OpenAI usage on the class key. Jenna runs it by hand. It is not run during implementation. Run it before the first release and whenever the list or the model
changes. Any package it flags gets added to the list.

**Size:** each build prints the installer size. The workflow fails above 1.9 GB, below GitHub's 2 GB per-file
limit. Expect several hundred packages including dependencies, and roughly 0.5–1.5 GB. Only the first build will
show the real number.

---

## 5. GitHub Actions workflows (`.github/workflows/`)

**One workflow, `build-desktop.yml`, with three jobs:** `mac`, `windows`, and
`release`, which needs both. With two separate workflows, each would create its own *draft* release for the same tag
(GitHub doesn't link a draft to its tag until it's published), and one platform failing would leave a half-filled
release. In one workflow, the `release` job runs only when both builds succeed. It downloads both artifacts and
creates the single draft. Delete `build-electron-mac.yml` and `build-electron-windows.yml`.

Both build jobs:
- **Triggers:** `push: branches: [uiuc_electron]`, `push: tags: ["uiuc-desktop-v*"]`, plus `workflow_dispatch`.
  The manual button only appears once the file is on the default branch (`main`). That's harmless.
- **`concurrency`:** one group per branch, with `cancel-in-progress` for branch pushes, so a new push cancels the
  older build. Never cancel tag builds.
- **Action versions:** don't use `actions/checkout@v4`, `setup-node@v4`, `cache@v4` or `upload-artifact@v4`, which
  run on the Node version GitHub is retiring. Bump each to its current Node 24 major, as `uiuc_shinylive` did
  (`checkout@v7` there). Look up the current major of each action when we implement.
- **Tag check:** keep, with the prefix changed to `uiuc-desktop-v` (tag must equal `electron/package.json`
  version).
- **Write `electron/.env`:** `OPENAI_API_KEY=${{ secrets.DOG_LOVER }}`. **Fail the build if the secret is empty**,
  for example on a fork or with a missing secret. The step never echoes the value, and `set -x` is not used in it.
- **Replace the package step** with the new `install_packages.R` (§4).
- **Cache:** key on the OS, R version, `install_packages.R` and `DESCRIPTION`. Save on `uiuc_electron` as well as
  `main`. Tag builds can't read branch caches (GitHub scoping), so release builds install from scratch. That's
  slower, but always clean.
- **Build and install RTutor** into the bundled library: keep.
- **Remove** "Stage app sources into electron/app" (no `app.R`) and the `hgu133plus2.db` check.
- **"Test bundled R runtime":** keep. Change `required` to `RTutor`, `shiny`, `rmarkdown`, `tidyverse`, `caret`,
  `plotly`, `canvasXpress` and a sample of the list. Also run `RTutor::run_app()` with `RTUTOR_DESKTOP=1` and a
  dummy `OPENAI_API_KEY`, and check that `rmarkdown::pandoc_available()` sees the bundled pandoc.
- **Pandoc staging:** keep. Check that pandoc 3.11 still exists at that URL when we implement.
- **Size report and 1.9 GB guard** after the installer is built.
- **Upload artifact:** keep, with retention raised from 3 to 14 days so a test build can be downloaded and tried on
  a real machine.
- **Publish (the `release` job):** only on `uiuc-desktop-v*` tags, using `softprops/action-gh-release@v2` with:
  - the default `GITHUB_TOKEN` (`permissions: contents: write`) and no `repository:`/`token:` overrides
  - `name: UIUC RTutor Desktop <version>`
  - `body_path: electron/RELEASE_NOTES.md`: minimum systems, short install steps, and a link to the install guide **at that tag**
    (`https://github.com/gexijin/RTutor/blob/<tag>/electron/INSTALL.md`). That link and its screenshots keep
    working after branches are merged or deleted.
  - `make_latest: false`
  - **`draft: true`** (Jenna publishes after testing)

**`mac` job, specific changes:**
- No certificate, keychain, notarization or Homebrew libomp steps. CRAN binaries use the libomp that ships inside R.framework, and
  `verify_no_host_deps_mac.sh` catches anything left pointing at the build machine.
- Keep:
  - disk cleanup
  - the R.framework staging and `patch_r_mac.sh`
  - `relocate_install_names_mac.sh`, run after packages are installed
  - `verify_no_host_deps_mac.sh`, run on both the staging tree and the packaged app
  - pruning docs and tests
  - raising file-descriptor limits
  - the two-phase `--dir` → DMG build with an explicit `dmg.size` (without it the DMG step runs out of space)
  - `codesign --verify --deep --strict`, which works with ad-hoc signatures
- Runner: `macos-26` (arm64).

**`windows` job, specific changes:**
- No code-signing steps.
- Keep: Rtools setup (some packages may still need it), R.win staging with robocopy, pandoc, and the NSIS build.
- Runner: `windows-2025`.

---

## 6. Release process and the semester key swap (runbook, goes into `electron/README.md`)

1. Your boss sets the new semester key as the value of the `DOG_LOVER` secret on gexijin/RTutor.
2. On `uiuc_electron` (or `uiuc_main` after the merge): bump `electron/package.json`, commit, and tag
   `uiuc-desktop-vX.Y.Z`. The exact commands are in `electron/README.md` (see §12).
3. Push the branch and the tag. This starts two builds, one for the branch push (test artifacts) and one for the
   tag (the release). The extra branch build is harmless and free on a public repo.
4. The workflow builds both installers (about an hour) and attaches the `.dmg` and `.exe` to one **draft** release.
5. Download both. Run the §9 checklist on a real Apple Silicon Mac and a Windows PC.
6. Publish the draft. Send students the release link and the install guide.
7. Release the new build **before** the old key expires. Old installs show the expiry popup once their key stops
   working.

---

## 7. Student install guide (`electron/INSTALL.md`, plus a short `RELEASE_NOTES.md`)

`RELEASE_NOTES.md` (the text of each release page) opens with **"Requirements: Mac with Apple Silicon (M1 or newer)
running macOS 12 Monterey or later, or a 64-bit Windows 10/11 PC. Intel Macs, Chromebooks and Linux are not
supported."** Then come the download file for each system, the short install steps, and the link to the full
guide.

Written step by step, with places marked `![screenshot: ...]` for Jenna's screenshots. Images go in
`electron/install-images/`.

**macOS** (Apple Silicon, macOS 12 Monterey or later, which Electron 39 requires):
1. Download `UIUC-RTutor-X-mac-arm64.dmg` from the release page.
2. Open the DMG and drag UIUC RTutor to Applications.
3. Open it from Applications. macOS says it "cannot verify UIUC RTutor is free of malware". Click **Done** (not
   Move to Trash).
4. Open System Settings → Privacy & Security, scroll to "UIUC RTutor was blocked…", and click **Open Anyway**.
   Enter your Mac password. Click **Open Anyway** again.
5. The first launch takes about 30 s (splash screen). Later launches are faster.
6. **If stuck** (for example a "damaged" message): open Terminal and paste
   `xattr -dr com.apple.quarantine "/Applications/UIUC RTutor.app"`, then open the app again.
7. University-managed Macs may block unsigned apps entirely. Use a personal computer or contact IT.
8. **Updating each semester:** download the new DMG, replace the app in Applications, and repeat steps 3–4.

**Windows** (64-bit Windows 10 or 11):
1. Download `UIUC-RTutor-X-win-x64.exe`.
2. If the browser warns that the file "isn't commonly downloaded": Edge → **⋯ → Keep → Show more → Keep anyway**;
   Chrome → **Keep**.
3. Run it. SmartScreen shows "Windows protected your PC". Click **More info** → **Run anyway**.
4. It installs for your user only (no admin password) and opens. Start-menu and desktop shortcuts are added.
5. If antivirus quarantines it, restore or allow it in your antivirus. School-managed PCs may block it.
6. **Updating:** run the new installer. It replaces the old version.
7. **Windows on ARM laptops** (Snapdragon, some Surface models) run the x64 build under emulation. It should work
   but more slowly. Not tested.

Both: a troubleshooting section that covers where the log files are, "stuck on splash", "expired" popup → download
new version, and "package not included" → rephrase the request.

---

## 8. Things deliberately left out
- Code signing and notarization. Add later if Mac support requests get heavy: an Apple Developer account
  ($99/year) plus signing and notarization steps in the workflow.
- Auto-update that installs in place. Students are told about new versions and download them by hand.
- Bioconductor, Python (`reticulate`), and per-user usage limits.
- Linux and Intel Mac builds.
- Usage logging and feedback storage on desktop (no SQLite DB; code already skips it).

---

## 9. Testing and acceptance

**Automated (CI):** the R runtime test, the host-dependency sweep (Mac), the signature check (Mac), the size guard,
and the package install failing on anything missing.

**Local, before any CI work ("Phase 0"):** `RTUTOR_DESKTOP=1 Rscript -e 'RTutor::run_app()'`-style launch from a
plain R session works. `devtools::test()` passes apart from the 3 known `mod_16_qa` failures. Jenna then runs the
coverage check (§4) herself.

**Manual, on a real Apple Silicon Mac and a real Windows PC, for every release:**
- [ ] Downloaded from the release page (so the quarantine and SmartScreen warnings actually appear), the install
      guide's steps work exactly as written
- [ ] Launches on a machine with **no R installed**
- [ ] Launches on a machine that **has** a different R installed (isolation)
- [ ] Double launch → one window
- [ ] Quit → no leftover `Rscript`/`R` process (Activity Monitor / Task Manager)
- [ ] A demo dataset plus a plotting prompt works. A modelling prompt (`lm`, `t.test`) works. A `caret` or
      `randomForest` prompt works.
- [ ] Uploading a CSV and an Excel file works
- [ ] The HTML report download renders (pandoc) and the EDA report renders
- [ ] Asking for a package that isn't bundled shows the "not included" message
- [ ] Wi-Fi off → readable "could not reach the AI server" popup
- [ ] Expired-key popup: test once with a deliberately bad key and confirm the 401 popup and its link. Assumption to
      confirm: OpenAI returns 401 for an *expired* project key, as it does for a revoked one. Check this with your
      boss's key settings, or with a test key set to expire soon.
- [ ] An older installed version shows the update dialog when a newer release exists
- [ ] Log file exists, and its path is shown on the splash and error screens

---

## 10. Order of work
1. §1 branch and ignore files → §2 R changes and tests → local Phase 0 check (no paid LLM calls) → commit.
2. §4 package list and the coverage-check script (written, not run). Jenna runs it, and the list is adjusted to
   match.
3. §3 Electron shell. Try `npm start` locally with the system R if possible (WSL can't run the Mac or Windows
   GUI, so this may wait for CI).
4. §5 workflow, `mac` job first (the `windows` job is temporarily disabled with `if: false`) → push → fix until
   green → download the test DMG → Jenna tries it on a Mac.
5. Enable the `windows` job → same.
6. §7 install guide, then Jenna's screenshots.
7. First tag `uiuc-desktop-v1.0.0` → draft release → §9 checklist → publish.
8. Merge `uiuc_electron` into `uiuc_main`. Retire `uiuc_shinylive`.

---

## 11. Open questions
Answered (see §0): icon, draft releases, LLM-check cost, failing tests.

None. Repo access was confirmed on 2026-10-06:
- Jenna has push permission on gexijin/RTutor.
- Actions run there (the shinylive deploys).
- No rulesets block tag pushes.
- The `DOG_LOVER` secret exists.

---

## 12. Changes made during implementation

- **R 4.5.3, not "the current release".** R 4.6 (current) only supports macOS 14 Sonoma or later, while R 4.5 supports
  macOS 11+. Jenna chose 4.5.3, so the Mac minimum stays macOS 12 (Electron's floor).
- **Mac packages come from the same pinned snapshot as Windows.** Posit Package Manager serves arm64 macOS binaries
  for R 4.5, so both platforms install identical versions from one `CRAN_SNAPSHOT_DATE`. CRAN's own Mac binaries
  aren't used. All 116 listed names were checked to exist as binaries on both platforms.
- **`on_desktop()` is a function.** Package-level code runs once at install time (on the build machine), so a
  constant would always be FALSE.
- **`bootstrap.R` doesn't redirect output to a file.** R can't copy messages to two places, and redirecting them would
  hide Shiny's "Listening on" line, which `main.js` waits for. `main.js` already logs all R output to
  `uiuc-rtutor-electron.log`.
- **No `.npmrc`; releases are tagged by hand.** Tested: in a subfolder, `npm version` bumps the number but creates
  neither a commit nor a tag. The exact commands are in `electron/README.md`.
- **One workflow file** (`build-desktop.yml`) with `mac`, `windows` and `release` jobs. Actions are on their
  current Node 24 majors (checkout v7, setup-node v7, cache v6, upload-artifact v7, download-artifact v8,
  action-gh-release v3). `actionlint` reports no problems.
- **The Mac runtime test hides the runner's R** (`/Library/Frameworks/R.framework` and `/opt/R`) while it runs, so it
  proves the bundle works on a Mac without R.
- **`summarytools` may not load on Macs.** It imports `tcltk`, which needs CRAN's optional Tcl/Tk under `/opt/R`. Only
  the EDA tab uses it, and that tab is hidden in UIUC RTutor. The runtime test reports it as a warning rather than a
  failure.
- **Coverage-check prompts:** `uiuc_demo_questions.csv` holds a single real prompt (the rest are jokes). The check
  therefore runs the ~118 prompts in `demo_questions.csv` plus 10 typical course requests against each of the 13 UIUC
  datasets, 243 prompts in all.
- **External links open in the system browser** (`setWindowOpenHandler`), so the "expired" popup's link works.
  Links back to the app (report downloads) keep Electron's default behavior.
- **Windows `R_USER`** points at the writable data folder (§3.2).
