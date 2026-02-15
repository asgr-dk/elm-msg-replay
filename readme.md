# Elm Message Replay

Reliable hot-reloading for Elm programs!

It is sheer delight to build graphics without having to get the application back
into a certain state in between making changes.

Another really great aspect of replay, is how programs behaves when the
update-function is modified. If you run through the correct set of actions to
reach a certain state, but the update-logic is not quite there, you can simply
tweak the update-function of your program, and reload the program to see if
you're done yet.

Replay runs until the last recognized message is decoded and then starts the
program.

## Example

```bash
cd example
elm make --output/build/main.js source/Main.elm
elm reactor # http://localhost:8000/source/Main.html
```

## Links

- previous implementation:
  https://package.elm-lang.org/packages/opvasger/msg-replay/latest/MsgReplay
