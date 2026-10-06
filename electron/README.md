# UIUC RTutor Desktop: maintainer notes

> This branch is the **desktop version** of UIUC RTutor. `uiuc_main` (server) and `uiuc_shinylive` (browser) are
> separate versions. Copy shared app changes between them with `git cherry-pick`; don't merge the branches.

The desktop app is an Electron shell around a bundled, portable R with RTutor and a fixed set of packages
installed. `main.js` starts `Rscript --vanilla bootstrap.R`, which serves `RTutor::run_app()` on
`127.0.0.1:<free port>`, and shows it in a window. Students' guide: [INSTALL.md](INSTALL.md).
Design and decisions: [PLAN.md](PLAN.md).

## Files

| File | Role |
|---|---|
| `main.js` | Starts R, splash and progress, single-instance lock, kills R on quit, logging |
| `bootstrap.R` | Sets the bundled library and runs `RTutor::run_app()` |
| `updater.js` | Tells students when a newer `uiuc-desktop-v*` release exists |
| `splash.html` | Loading screen |
| `package.json` | Version (the release version), electron-builder config |
| `scripts/install_packages.R` | **The bundled package list** and the pinned package snapshot date |
| `scripts/check_package_coverage.R` | Paid LLM check: packages generated code uses that the bundle lacks |
| `scripts/test_runtime.R` | CI check that the bundled R, RTutor, packages and pandoc all work |
| `scripts/*_mac.sh` | Make the bundled macOS R independent of any R on the student's Mac |
| `scripts/get_r_mac.sh`, `get_r_windows.ps1` | Build a local runtime for `npm start` on a Mac or Windows PC |
| `RELEASE_NOTES.md` | Text of each GitHub release (`{{TAG}}` is filled in by the build) |
| `../.github/workflows/build-desktop.yml` | Builds both installers; drafts the release on a tag |

## How the API key gets in

The `DOG_LOVER` secret on gexijin/RTutor holds the class's OpenAI `sk-` key. The build writes it to `electron/.env`
as `OPENAI_API_KEY`. That file is packaged into the app, and `main.js` passes it to R. The R code reads only
`OPENAI_API_KEY` (`create_response()` in `R/fct_helpers.R`). `main.js` also sets `RTUTOR_DESKTOP=1`, which stops
generated code from installing packages. A key embedded this way can be extracted by anyone with the installer.
The key's spending cap and semester expiry are what limit the damage.

## Release a new version (each semester, or for a fix)

1. For a new semester: your boss puts the new key into the `DOG_LOVER` secret (repo Settings → Secrets and
   variables → Actions).
2. Commit any changes on `uiuc_electron`. Then bump the version, commit it, and push
   a matching `uiuc-desktop-vX.Y.Z` tag. (`npm version` can't do the git part here, because `package.json` isn't at
   the repo root.)
   ```bash
   cd electron
   npm version minor --no-git-tag-version   # new semester or features; `patch` for a small fix
   V=$(node -p "require('./package.json').version")
   cd ..
   git add electron/package.json electron/package-lock.json
   git commit -m "UIUC RTutor Desktop $V"
   git tag "uiuc-desktop-v$V"
   git push origin HEAD "uiuc-desktop-v$V"
   ```
3. The **Build UIUC RTutor Desktop** workflow builds the Mac and Windows installers (about an hour, longer for a
   tag because tag builds don't use the package cache). When both pass, it creates a **draft** release.
4. Download both installers from the draft (Releases page → the draft → Assets). Run the checklist in
   [PLAN.md §9](PLAN.md#9-testing-and-acceptance) on a real Apple Silicon Mac and a Windows PC.
5. Publish the draft: Edit → **Publish release**. Leave "Set as the latest release" **unchecked**.
6. Send students the release link. Do this **before** the old key expires: old installs show "this version has
   expired" once their key stops working.

Pushes to `uiuc_electron` also build test installers. Download them from the workflow run's **Artifacts** section
(kept 14 days).

## Change the bundled packages

Edit `server_list` / `course_list` in `scripts/install_packages.R`. `course_list` is ordered from most to least
commonly used. If the installer is over the size limit (the build fails above 1,900 MB), remove packages from the
**end** of `course_list`. To pick up newer package versions, bump `CRAN_SNAPSHOT_DATE`. Any change to that file
reinstalls the whole library on the next build.

Then check coverage (paid, about $2–3 on the class key), from the repo root:
```bash
Rscript electron/scripts/check_package_coverage.R coverage.csv
```
It prints every package the generated code used that isn't bundled. Add those to `course_list`.

## Run locally (Mac or Windows)

```bash
bash electron/scripts/get_r_mac.sh            # Mac (or: pwsh electron/scripts/get_r_windows.ps1)
cp electron/.env.example electron/.env        # then paste the key into electron/.env
cd electron && npm ci && npm start
```
`get_r_*` installs the full package set, which takes a while. Without a bundled runtime, `npm start` falls back to
the `Rscript` on your PATH and your own R library (development only).

## Screenshots for INSTALL.md

The Windows steps use the screenshots in `electron/pngs/` (`win-1.png` to `win-6.png`). The Mac and browser-warning
steps still have `<!-- SCREENSHOT: ... -->` placeholders. To add one, save the image in `electron/pngs/` and
replace the comment with `![description](pngs/<name>.png)`.
