The StandAloneDeriving compiler directive is necessary to create
an orphan instance of Ord for SDL.Keysym

> {-# LANGUAGE StandaloneDeriving #-}

> module Main
>        ( main
>        ) where

We want to hide `null` and `filter` from the Prelude since we are using
similarly named functions from Data.Set

> import Prelude hiding (null, filter)

Control.Monad

> import qualified Control.Monad as M

We want to store the keyboard input in a Set for easy processing

> import Data.Set (Set, empty, insert, delete, null, filter)

For pedagogical purposes, import SDL as SDL. We need to explicitly
import the SDL Keyboard module because it is not included by default.

> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.Keyboard as SDLK

For pedagogical purposes, import OpenGL as GL.

> import qualified Graphics.Rendering.OpenGL as GL

We must make SDL.Keysym an instance of Ord to use it in a Set.

> deriving instance Ord SDL.Keysym

----------------------------------------------------------------------

> main :: IO ()
> main = do

Set constants for use in window creation

>   let title     = "2-OpenGL-Context-Creation"
>       position  = SDL.Position 0 0
>       size      = SDL.Size 640 480
>       win_flags = [SDL.WindowOpengl]

Create the window

>   window <- SDL.createWindow title position size win_flags

Initialize OpenGL

>   initGL

Set up a rendering context

>   renderer <- SDL.glCreateContext window

Run main loop

>   mainLoop window empty

Destroy the rendering context

>   SDL.glDeleteContext renderer

Destroy the window

>   SDL.destroyWindow window

----------------------------------------------------------------------

Set OpenGL context attributes

> initGL :: IO ()
> initGL = do

Just for convenience, let's define a shorthand for setting attributes.

>   let setAttr = SDL.glSetAttribute

Unfortunately, the current implementation of glSetAttribute expects a
CInt as the second argument, but the Profile constants must be retrieved
by a call to sdlGLAttributeToC. I'd like to find a more elegant way to
solve this issue, but this is a quick solution that works for now in my
fork of the high-level SDL 2.x bindings.

>       profileVersion = SDL.sdlGLAttributeToC SDL.GLContextProfileCore

OpenGL profile and version

>   setAttr SDL.GLContextProfileMask profileVersion
>   setAttr SDL.GLContextMajorVersion 3
>   setAttr SDL.GLContextMinorVersion 2

Enable double buffering with a 24 bit Z buffer

>   setAttr SDL.GLDoubleBuffer 1
>   setAttr SDL.GLDepthSize 24

----------------------------------------------------------------------

> mainLoop window keysDown = do

Capture any new events

>   keysDown' <- parseEvents keysDown

Quit if Escape is pressed or the user X's out the window.

>   M.unless (keyDown SDL.Escape keysDown') $ do
>     mainLoop window keysDown'

----------------------------------------------------------------------

This is a convenience function for checking whether a key exists in the Set.

> keyDown :: SDL.Scancode -> Set SDL.Keysym -> Bool
> keyDown k = not . null . filter ((== k) . SDL.keyScancode)

----------------------------------------------------------------------

Use pollEvent to return the topmost event from the queue. If there are
no events in the queue, pollEvent will return Nothing.

> parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
> parseEvents keysDown = do
>   maybe_event <- SDL.pollEvent
>   case maybe_event of

Nothing happened

>     Nothing -> return keysDown

Something happened

>     (Just event) ->
>       case SDL.eventData event of

Add a key to the set

>         SDL.Keyboard SDL.KeyDown _ _ k -> parseEvents (insert k keysDown)

Remove a key from the set

>         SDL.Keyboard SDL.KeyUp _ _ k -> parseEvents (delete k keysDown)

Add Escape to the set on a Quit event to quit the game

>         SDL.Quit -> do
>           keyQuit <- SDLK.getKeyFromScancode SDLK.ScancodeEscape
>           return (insert (SDL.Keysym SDL.Escape keyQuit 0) keysDown)

Otherwise, continue

>         _ -> parseEvents keysDown
