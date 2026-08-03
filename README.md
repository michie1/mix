# Mix

Mix is a small Elm app for building a DJ playlist from a track library. It finds tracks with a close BPM and compatible musical key, then shows the best matches first.

## Use

1. Search or browse the Library.
2. Select a track to see its matches.
3. Use **Add** to place a track at the end of the Playlist.
4. Select any playlist track to find the next match.
5. Use **Remove** to take a track out of the Playlist.

The playlist also shows the pitch change at the start and end of each track.

## Run locally

Install the project packages, then start the development server:

```sh
yarn
yarn dev
```

Open `http://localhost:3001`.

## Project files

1. `src/Main.elm` contains the app state, matching logic, and views.
2. `static/index.html` contains the page shell and styles.
3. `static/tracks.json` contains the track library.

The app uses Elm 0.19.2 and Webpack 5.
