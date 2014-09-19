Part 1: SDL Setup
=================

Getting Started
---------------

First, you need to install SDL 2. On OSX you can install it through homebrew using `brew install sdl2`. On GNU/Linux it should be available through your package manager. Otherwise, you can always download the source or binaries from the [official website](http://www.libsdl.org/download-2.0.php).

Next, you must set up a cabal sandbox with the experimental SDL 2 bindings. Sandboxes were added in Cabal 1.18, so use `cabal --version` to check your version. If you don't have at least 1.18, either grab the latest Haskell Platform or use `cabal update cabal-update`. In your terminal window:

Grab the required code from GitHub.

```
git clone git@github.com:ericnething/opengl-sdl-tutorial.git opengl-sdl-tutorial
git clone git@github.com:ericnething/hsSDL2.git hssdl2
```

Create the cabal sandbox and set up the dependencies.

```
cd opengl-sdl-tutorial
cabal sandbox init
cabal sandbox add-source ../hssdl2
cabal install --only-dependencies
cabal build
```

Creating a Window
-----------------

To create a window using SDL, you must provide a title, a position, a size, and a list of window flags (options).

```haskell
window <- SDL.createWindow title position size win_flags
```

Then call your main program loop.

```haskell
mainLoop window arg2 arg3
```

Finally, destroy the window when the main program finishes.

```haskell
SDL.destroyWindow window
```

There is a lot of error-checking and other initialization that is taken care of by the Haskell bindings to SDL 2 that you don't need to worry about. All you need are these three lines in your main do-block.

Capturing Input
---------------

We will use `SDL.pollEvent` to grab the most recent event from the event queue. We can keep track of the state of the keyboard by storing the pressed keys in a Set. When the key is pressed down, it is added to the Set. When the key is released, it is removed from the Set. If the event queue is empty, we pass the unmodified Set back to the program. We also want to be able to quit the program when the user X's out the window or presses `Esc`, so we will also check for a `SDL.Quit` event. Let's handle all of this in our custom `parseEvents` function.

```haskell
parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
```

Our `keyDown` function checks if a specific key is in the Set.

```haskell
keyDown :: SDL.Scancode -> Set SDL.Keysym -> Bool
```

