'use strict';

const { Elm } = require('../src/Main.elm');

const storageKey = 'mix-playlists';
const legacyStorageKey = 'mix-playlist';

function loadPlaylist() {
  try {
    const saved = localStorage.getItem(storageKey);

    if (saved) {
      return JSON.parse(saved);
    }

    return JSON.parse(localStorage.getItem(legacyStorageKey)) || [];
  } catch (_error) {
    return [];
  }
}

const app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: loadPlaylist()
});

app.ports.savePlaylist.subscribe((playlist) => {
  try {
    localStorage.setItem(storageKey, JSON.stringify(playlist));
  } catch (_error) {
    // Storage can be unavailable in private browsing or restricted contexts.
  }
});

app.ports.exportMarkdown.subscribe((markdown) => {
  const blob = new Blob([markdown], { type: 'text/markdown;charset=utf-8' });
  const url = URL.createObjectURL(blob);
  const link = document.createElement('a');

  link.href = url;
  link.download = 'mix-playlist.md';
  document.body.appendChild(link);
  link.click();
  link.remove();
  URL.revokeObjectURL(url);
});
