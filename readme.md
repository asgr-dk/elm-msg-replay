# Elm Message Replay

Reliable hot-reloading for Elm programs!

Replay runs until the last recognized message is decoded and then starts the
program.

## Example

```bash
cd example
elm make --output/build/main.js source/Main.elm
elm reactor # http://localhost:8000/source/Main.html
```
