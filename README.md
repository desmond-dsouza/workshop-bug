# Snake Game in Elm

## Build

* Game starts in `Main.elm`
* Must be viewed via a web server such as `elm-live`
* Skeleton html is `game.html`, which expects to import `main.js`

So:

`elm-live src/Main.elm -s game.html -- --output main.js`

## Deployment

Can deploy this to Zeit/now with `game.html` as the default html file

