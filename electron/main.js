// main.js: UIUC RTutor desktop shell.
// Starts the bundled R, which serves RTutor on 127.0.0.1:<free port>, then shows it in a window.
// Each workaround below fixes a known problem with running a bundled R from a desktop app.
const { app, BrowserWindow, dialog, Menu, shell } = require('electron');
const path = require('path');
const fs = require('fs');
const { spawn } = require('child_process');
const net = require('net');
const dotenv = require('dotenv');
const { checkForUpdates } = require('./updater');

let childProc = null;

// ---------- shutdown handlers ----------
// Registered at module top-level so they fire even if the user quits during
// startup (before createWindow finishes), which would otherwise orphan Rscript.
app.on('before-quit', () => { app.isQuitting = true; safeKill(childProc); });
app.on('window-all-closed', () => app.quit());

// ---------- logging ----------
const LOG_FILE = path.join(app.getPath('temp'), 'uiuc-rtutor-electron.log');
function log(...args) {
  try {
    const line = args.map(x => (typeof x === 'string' ? x : JSON.stringify(x))).join(' ');
    fs.appendFileSync(LOG_FILE, line + '\n');
    console.log(line);
  } catch {}
}

// ---------- crash guards ----------
process.on('uncaughtException', (err) => {
  const msg = (err && err.stack) ? err.stack : String(err);
  log('[uncaughtException]', msg);
  try { dialog.showErrorBox('Uncaught Exception', msg + `\n\nLog: ${LOG_FILE}`); } catch {}
});
process.on('unhandledRejection', (reason) => {
  log('[unhandledRejection]', (reason && reason.stack) ? reason.stack : String(reason));
});

// ---------- single instance ----------
// A second launch focuses the existing window instead of starting a second R.
const gotLock = app.requestSingleInstanceLock();
if (!gotLock) app.quit();
else app.on('second-instance', () => {
  if (global.win) {
    if (global.win.isMinimized()) global.win.restore();
    global.win.focus();
  }
});

// ---------- API key ----------
// CI writes electron/.env from the DOG_LOVER secret. dotenv.parse (not config) so the
// student's own environment can never override or supply the key.
function readApiKey() {
  try {
    const env = dotenv.parse(fs.readFileSync(path.join(__dirname, '.env')));
    return (env.OPENAI_API_KEY || '').trim();
  } catch {
    return '';
  }
}

// ---------- helpers ----------
// Locate Rscript plus the env vars that make the bundled R independent of any R on the machine.
// `dataDir` is the writable per-user folder (the install folder is read-only).
function getRuntime(dataDir) {
  const rp = app.isPackaged ? process.resourcesPath : __dirname;

  if (process.platform === 'win32') {
    const R_ROOT = path.join(rp, 'runtime', 'R.win');
    const binDir = path.join(R_ROOT, 'bin');
    const rscript = path.join(binDir, 'Rscript.exe');
    if (!fs.existsSync(rscript)) return devFallback(rscript);
    // rmarkdown shells out to pandoc, which is not an R package, so it is bundled
    // separately. RSTUDIO_PANDOC is what rmarkdown::find_pandoc() checks first.
    const pandocDir = path.join(rp, 'runtime', 'pandoc.win');
    return {
      rscript,
      lib: path.join(R_ROOT, 'library'),
      env: {
        R_HOME: R_ROOT,
        // R_USER is what `~` means in R on Windows; keep it writable, not the read-only install folder.
        R_USER: dataDir,
        RSTUDIO_PANDOC: pandocDir,
        PATH: [binDir, pandocDir, process.env.PATH || ''].filter(Boolean).join(';'),
      },
    };
  }

  if (process.platform === 'darwin') {
    const R_RES = path.join(rp, 'runtime', 'R.framework', 'Resources');
    const rscript = path.join(R_RES, 'bin', 'Rscript');
    if (!fs.existsSync(rscript)) return devFallback(rscript);
    const pandocDir = path.join(rp, 'runtime', 'pandoc.mac');
    return {
      rscript,
      lib: path.join(R_RES, 'library'),
      env: {
        R_HOME: R_RES,
        // Rscript reads RHOME, not R_HOME; without it it execs the
        // compile-time /Library/Frameworks path and exits 255.
        RHOME: R_RES,
        DYLD_FALLBACK_LIBRARY_PATH: path.join(R_RES, 'lib'),
        RSTUDIO_PANDOC: pandocDir,
        PATH: [path.join(R_RES, 'bin'), pandocDir, process.env.PATH || ''].filter(Boolean).join(':'),
      },
    };
  }

  return devFallback(null);
}

// Unpackaged `npm start` without a bundled runtime: use the developer's own Rscript and library.
function devFallback(expected) {
  if (!app.isPackaged) {
    log('[R runtime] no bundled R, using Rscript from PATH (dev only)');
    return { rscript: process.platform === 'win32' ? 'Rscript.exe' : 'Rscript', lib: null, env: {} };
  }
  const msg = `Could not find the bundled R.\nExpected at: ${expected}\n\nLog: ${LOG_FILE}`;
  log('[FATAL]', msg);
  try { dialog.showErrorBox('R Not Found', msg); } catch {}
  return null;
}

// "If there's no process to kill, OR there is one but we already killed it, bail out."
function safeKill(proc) {
  if (!proc || proc.killed) return;
  try {
    if (process.platform === 'win32') {
      // SIGTERM doesn't reliably kill R on Windows — httpuv ignores it.
      // taskkill /T kills the entire process tree (Rscript + child R).
      spawn('taskkill', ['/pid', String(proc.pid), '/T', '/F'], { windowsHide: true });
    } else {
      proc.kill('SIGTERM');
    }
  } catch (e) {
    log('[safeKill]', e && e.message ? e.message : String(e));
  }
}

async function waitForHttp(url, { timeoutMs = 120000, intervalMs = 1000 } = {}) {
  const start = Date.now();
  let attempts = 0;
  while (Date.now() - start < timeoutMs) {
    attempts++;
    try {
      const ctrl = new AbortController();
      // The first response from Shiny can take 10-30 s. A short abort here
      // killed every attempt before Shiny could answer.
      const to = setTimeout(() => ctrl.abort(), 30000);
      const res = await fetch(url, { method: 'GET', signal: ctrl.signal });
      clearTimeout(to);
      // Any HTTP response proves the server is alive, even a 500 during startup.
      log(`[waitForHttp] attempt ${attempts}: got HTTP ${res.status}`);
      return true;
    } catch (err) {
      if (attempts <= 3 || attempts % 10 === 0) log(`[waitForHttp] attempt ${attempts}: ${err.name}: ${err.message}`);
    }
    await new Promise(r => setTimeout(r, intervalMs));
  }
  throw new Error(`Timeout waiting for ${url}`);
}

function getFreePort(start = 7777, end = 7999) {
  return new Promise((resolve, reject) => {
    const server = net.createServer();
    server.unref();
    server.on('error', () => {
      if (start < end) resolve(getFreePort(start + 1, end));
      else reject(new Error('No free ports'));
    });
    server.listen(start, '127.0.0.1', () => {
      const port = server.address().port;
      server.close(() => resolve(port));
    });
  });
}

// Push progress to both the taskbar/dock and the splash page; never throws.
function setSplashProgress(progress, statusText) {
  if (!global.win) return;
  try {
    global.win.setProgressBar(typeof progress === 'number' && progress >= 0 && progress <= 1 ? progress : -1);
    const pct = typeof progress === 'number' ? Math.round(Math.max(0, Math.min(1, progress)) * 100) : 0;
    global.win.webContents
      .executeJavaScript(`window.updateSplash && window.updateSplash(${pct}, ${statusText ? JSON.stringify(statusText) : 'null'})`)
      .catch(() => {});
  } catch {}
}

// Keep-alive: nudge the server every 30 s so an idle Shiny socket stays warm.
// Guards against grey-screen disconnects on idle desktops; cheap and harmless.
const HEARTBEAT_JS = `(function () {
  if (window.__rtutorHeartbeat) return;
  window.__rtutorHeartbeat = setInterval(function () {
    try {
      var send = window.Shiny && (Shiny.setInputValue || Shiny.onInputChange);
      if (send) send.call(Shiny, '.rtutorHeartbeat', Date.now(), { priority: 'event' });
    } catch (e) {}
  }, 30000);
})();`;

function showSplash(appOrigin) {
  global.win = new BrowserWindow({
    title: 'UIUC RTutor',
    width: 1200,
    height: 800,
    show: true,
    webPreferences: {
      contextIsolation: true,
      nodeIntegration: false,
      // Keep timers running while minimized so the heartbeat keeps firing.
      backgroundThrottling: false,
    },
  });
  const wc = global.win.webContents;
  // Keep "UIUC RTutor" as the window title. Otherwise Electron copies each page's <title>,
  // and the app's is shared with the server version ("RTutor 2.00").
  global.win.on('page-title-updated', (e) => e.preventDefault());
  wc.on('did-finish-load', () => wc.executeJavaScript(HEARTBEAT_JS).catch(() => {}));

  // External links (e.g. the "download the latest version" link) open in the system browser.
  // Links back to the app itself, such as report downloads, keep Electron's default behavior.
  wc.setWindowOpenHandler(({ url }) => {
    if (url.startsWith(appOrigin)) return { action: 'allow' };
    if (/^https?:\/\//.test(url)) shell.openExternal(url);
    return { action: 'deny' };
  });
  wc.on('will-navigate', (e, url) => {
    if (!url.startsWith(appOrigin) && /^https?:\/\//.test(url)) { e.preventDefault(); shell.openExternal(url); }
  });

  const html = fs.readFileSync(path.join(__dirname, 'splash.html'), 'utf8')
    .replace('{{LOG_FILE}}', LOG_FILE.replace(/\\/g, '/'))
    .replace('{{VERSION}}', app.getVersion());
  global.win.loadURL('data:text/html;charset=utf-8,' + encodeURIComponent(html));
}

function buildAppMenu() {
  const view = [{ role: 'reload' }, { role: 'forceReload' }];
  if (!app.isPackaged) view.push({ role: 'toggleDevTools' });
  view.push({ type: 'separator' }, { role: 'resetZoom' }, { role: 'zoomIn' }, { role: 'zoomOut' },
    { type: 'separator' }, { role: 'togglefullscreen' });
  return Menu.buildFromTemplate([
    process.platform === 'darwin' ? { role: 'appMenu' } : { label: 'File', submenu: [{ role: 'quit' }] },
    {
      label: 'Edit',
      submenu: [
        { role: 'undo' }, { role: 'redo' }, { type: 'separator' },
        { role: 'cut' }, { role: 'copy' }, { role: 'paste' }, { role: 'selectAll' },
      ],
    },
    { label: 'View', submenu: view },
  ]);
}

// loadURL() can reject with ERR_ABORTED even when the page loads: Shiny replaces the
// first navigation with its own. Treat "the page finished loading" as success, and retry
// a few times before giving up.
async function loadAppURL(win, url, { attempts = 3, delayMs = 750 } = {}) {
  const wc = win.webContents;
  let finished = false;
  let mainFrameFail = null;
  const onFinish = () => { finished = true; };
  // -3 = ERR_ABORTED (a superseded navigation) is benign; anything else is a real failure.
  const onFail = (_e, errorCode, _desc, _url, isMainFrame) => {
    if (isMainFrame && errorCode !== -3) mainFrameFail = errorCode;
  };
  wc.on('did-finish-load', onFinish);
  wc.on('did-fail-load', onFail);
  try {
    for (let i = 1; i <= attempts; i++) {
      finished = false;
      mainFrameFail = null;
      try {
        await win.loadURL(url);
        return;
      } catch (e) {
        await new Promise((r) => setTimeout(r, delayMs));
        if (finished && mainFrameFail === null) return;
        log(`[loadURL] attempt ${i}/${attempts} failed: ${e && e.message ? e.message : String(e)}`);
        if (i === attempts) throw e;
      }
    }
  } finally {
    wc.removeListener('did-finish-load', onFinish);
    wc.removeListener('did-fail-load', onFail);
  }
}

function fatal(title, msg) {
  log('[FATAL]', title, msg);
  try { dialog.showErrorBox(title, `${msg}\n\nLog: ${LOG_FILE}`); } catch {}
  app.quit();
}

// ---------- startup ----------
async function createWindow() {
  const host = '127.0.0.1';
  const port = await getFreePort();
  const appURL = `http://${host}:${port}`;

  showSplash(appURL);
  log(`=== Launch ${new Date().toISOString()} version=${app.getVersion()} ===`);

  const apiKey = readApiKey();
  if (!apiKey) {
    return fatal('No API Key', 'This copy of UIUC RTutor has no API key. ' +
      'Please download the official release from the UIUC RTutor download page.');
  }

  // Always the per-user folder: students launch from the Dock or Start menu, and an
  // inherited working directory only causes surprises.
  setSplashProgress(0.1, 'Preparing data folder…');
  const dataDir = path.join(app.getPath('userData'), 'rtutor');
  try {
    fs.mkdirSync(dataDir, { recursive: true });
  } catch (e) {
    return fatal('Data Folder Error', `Could not create ${dataDir}\n${e.message}`);
  }

  const runtime = getRuntime(dataDir);
  if (!runtime) { app.quit(); return; }
  const bootstrapPath = path.join(__dirname, 'bootstrap.R');
  setSplashProgress(0.25, 'R runtime located…');

  log(`resourcesPath = ${process.resourcesPath}`);
  log(`dataDir       = ${dataDir}`);
  log(`Rscript       = ${runtime.rscript}`);

  setSplashProgress(0.35, 'Starting R…');
  const env = {
    ...process.env,
    ...runtime.env,
    RTUTOR_DESKTOP: '1',
    OPENAI_API_KEY: apiKey,
    RTUTOR_HOST: host,
    RTUTOR_PORT: String(port),
    RTUTOR_DATA_DIR: dataDir,
  };
  if (runtime.lib) {
    // Isolate the bundled R from any R the student already has: only the bundled library.
    env.R_LIBS_USER = runtime.lib;
    env.R_LIBS = '';
    env.R_LIBS_SITE = '';
  }
  try {
    childProc = spawn(runtime.rscript, ['--vanilla', bootstrapPath], { cwd: dataDir, env, windowsHide: true });
  } catch (e) {
    return fatal('R Launch Error', `Failed to start R: ${e && e.stack ? e.stack : String(e)}`);
  }

  let listening = false;
  childProc.stdout.on('data', d => log('[R stdout]', String(d).trim()));
  childProc.stderr.on('data', d => {
    const text = String(d);
    log('[R stderr]', text.trim());
    if (/Listening on http:\/\//.test(text)) listening = true;
  });
  childProc.on('error', e => log('[R spawn error]', e && e.message ? e.message : String(e)));
  childProc.on('close', (code, sig) => {
    log('[R exit]', `code=${code ?? 0}`, sig ? `sig=${sig}` : '');
    if (app.isQuitting || !global.win || global.win.isDestroyed()) return;
    const html = `<html><body style="font-family:sans-serif;padding:16px">
        <h2>UIUC RTutor stopped</h2>
        <p>R exited with code <b>${code ?? 0}</b> ${sig ? `(signal ${sig})` : ''}. Close and reopen the app to try again.</p>
        <p>If this keeps happening, send this log file to your instructor:</p>
        <pre style="white-space:pre-wrap">${LOG_FILE.replace(/\\/g, '/')}</pre>
      </body></html>`;
    global.win.loadURL('data:text/html;charset=utf-8,' + encodeURIComponent(html));
  });

  // Wait for Shiny's "Listening on" line: the only reliable sign it has bound the port.
  setSplashProgress(0.6, 'Starting RTutor…');
  const listenDeadline = Date.now() + 300000;
  while (!listening && Date.now() < listenDeadline) {
    if (childProc.exitCode !== null) return; // the 'close' handler shows the error page
    await new Promise(r => setTimeout(r, 500));
  }
  if (!listening) log('[startup] Shiny never reported listening; trying the port anyway');

  setSplashProgress(0.8, 'Connecting…');
  try {
    await waitForHttp(appURL);
  } catch (err) {
    log('[waitForHttp]', err && (err.stack || String(err)));
    safeKill(childProc);
    return fatal('Startup Timeout', 'RTutor did not respond within 2 minutes.');
  }

  setSplashProgress(0.9, 'Loading user interface…');
  try {
    await loadAppURL(global.win, appURL);
    setSplashProgress(-1, '');
    // Delayed so the GitHub request stays out of Shiny's startup.
    setTimeout(() => {
      checkForUpdates(global.win).catch(e => log('[update check]', e && e.message ? e.message : String(e)));
    }, 5000);
  } catch (e) {
    log('[loadURL error]', e && e.stack ? e.stack : String(e));
    try { dialog.showErrorBox('Load Error', `Failed to load ${appURL}\n\nLog: ${LOG_FILE}`); } catch {}
  }
}

app.whenReady().then(() => {
  log('[main] startup', 'electron=' + process.versions.electron, 'platform=' + process.platform + '/' + process.arch);
  Menu.setApplicationMenu(buildAppMenu());
  return createWindow();
});
