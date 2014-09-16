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

We must make SDL.Keysym an instance of Ord to use it in a Set.

> deriving instance Ord SDL.Keysym

----------------------------------------------------------------------

> main :: IO ()
> main = do

Set constants for use in window creation

>   let title     = "1-SDL-Setup"
>       position  = SDL.Position 0 0
>       size      = SDL.Size 640 480
>       win_flags = [SDL.WindowOpengl]

Create the window

>   window <- SDL.createWindow title position size win_flags

Run main loop

>   mainLoop window empty

Destroy the window

>   SDL.destroyWindow window

> mainLoop window keysDown = do

Capture any new events

>   keysDown' <- parseEvents keysDown

Quit if Escape is pressed or the user X's out the window.

>   M.unless (keyDown SDL.Escape keysDown') $ do
>     mainLoop window keysDown'

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
> 
> keyDown :: SDL.Scancode -> Set SDL.Keysym -> Bool
> keyDown k = not . null . filter ((== k) . SDL.keyScancode)
