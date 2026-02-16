# Elm Message Replay

Reliable hot reloading for Elm programs!

## Message Replay

The goal of message replay is to regenerate state reliably when iterating on
programs.

Instead of having to click through the program every time code has been changed,
message replay will keep the program state as close as possible to where you
want it to be in between code changes.

When a message is changed or removed, replay regenerates all state possible
until that message fails to decode, partially reproducing the previous state.

![](/demo.gif)

### How It Works

During initialization, messages are first read from flags and decoded into a
list of json values. These values are then decoded and used to update the model,
throwing away all commands produced along the way. This process runs until all
messages are decoded, or an unrecognized messages fails to decode. Then the
program starts.

During runtime, all calls to the update function ensure messages are about to be
saved, or schedules a task to do so after 1 second of timeout.

## Example

```bash
cd example
elm make --output/build/main.js source/Main.elm
elm reactor # http://localhost:8000/source/Main.html
```
