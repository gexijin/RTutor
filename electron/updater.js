const { app, dialog, shell } = require('electron');

// gexijin/RTutor also hosts the web app's releases, so look only at UIUC desktop tags
// rather than /releases/latest. Drafts are invisible to this anonymous request.
// Anonymous GitHub API calls are limited to 60/hour per IP; a failure here is silent and harmless.
const RELEASES_API = 'https://api.github.com/repos/gexijin/RTutor/releases?per_page=50';
const TAG_PREFIX = 'uiuc-desktop-v';

function parseVersion(v) {
  const main = String(v).replace(TAG_PREFIX, '').trim().split(/[-+]/)[0];
  const parts = main.split('.').map(p => parseInt(p, 10));
  if (parts.length === 0 || parts.some(n => !Number.isFinite(n))) return null;
  return parts;
}

function isNewer(latest, current) {
  const a = parseVersion(latest);
  const b = parseVersion(current);
  if (!a || !b) return false;
  for (let i = 0; i < Math.max(a.length, b.length); i++) {
    const av = a[i] || 0;
    const bv = b[i] || 0;
    if (av !== bv) return av > bv;
  }
  return false;
}

async function checkForUpdates(parentWin) {
  if (!app.isPackaged) return;
  if (parentWin && parentWin.isDestroyed()) return;

  const ctrl = new AbortController();
  const to = setTimeout(() => ctrl.abort(), 10000);
  let releases;
  try {
    const res = await fetch(RELEASES_API, {
      signal: ctrl.signal,
      headers: { 'Accept': 'application/vnd.github+json' },
    });
    if (!res.ok) return;
    releases = await res.json();
  } finally {
    clearTimeout(to);
  }

  const desktop = (Array.isArray(releases) ? releases : [])
    .filter(r => !r.draft && !r.prerelease && String(r.tag_name || '').startsWith(TAG_PREFIX));
  let newest = null;
  for (const r of desktop) {
    if (!newest || isNewer(r.tag_name, newest.tag_name)) newest = r;
  }

  const current = app.getVersion();
  if (!newest || !isNewer(newest.tag_name, current)) return;
  const latest = newest.tag_name.replace(TAG_PREFIX, '');

  const opts = {
    type: 'info',
    buttons: ['Download', 'Later'],
    defaultId: 0,
    cancelId: 1,
    title: 'Update available',
    message: `UIUC RTutor Desktop ${latest} is available (you have ${current}).`,
    detail: 'Download it from the release page and install it over this version.',
  };
  const result = parentWin
    ? await dialog.showMessageBox(parentWin, opts)
    : await dialog.showMessageBox(opts);
  if (result.response === 0 && newest.html_url) await shell.openExternal(newest.html_url);
}

module.exports = { checkForUpdates, isNewer };
