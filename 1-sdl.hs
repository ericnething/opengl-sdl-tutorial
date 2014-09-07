-- The StandAloneDeriving compiler directive is necessary to create
-- an orphan instance of Ordfor SDL.Keysym
{-# LANGUAGE StandaloneDeriving #-}

module Main
  ( main
  ) where

import Prelude hiding (null, filter)
import qualified Control.Monad as M
import Data.Set (Set, empty, insert, delete, null, filter)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Keyboard as SDLK

----------------------------------------------------------------------

winWidth = 640
winHeight = 480

----------------------------------------------------------------------

main :: IO ()
main =
  let title     = "Eric's SDL and OpenGL Tutorial"
      position  = SDL.Position 0 0
      size      = SDL.Size winWidth winHeight
      win_flags = [SDL.WindowOpengl]
  in SDL.withWindow title position size win_flags $ \window ->

    -- Setup a rendering context.
    let device    = SDL.FirstSupported
        ren_flags = [SDL.Accelerated]
    in SDL.withRenderer window device ren_flags $ \renderer ->
       render renderer empty

----------------------------------------------------------------------

-- primary rendering action
render screen keysDown = do

  -- capture any new events
  keysDown' <- parseEvents keysDown
  
  -- Set the drawing color to black
  SDL.setRenderDrawColor screen 10 10 10 255
  
  -- Clear the buffer, using the color set above.
  SDL.renderClear screen
  
  -- Set the drawing color to light blue.
  SDL.setRenderDrawColor screen 101 208 246 255 -- 0 153 204 255
  
  -- Create the ball
  SDL.renderFillRect screen $ SDL.Rect 100 100 20 20
  
  -- Swap our buffer for the present screen buffer, displaying it.
  SDL.renderPresent screen

  -- run at 16 fps
  SDL.delay (1000 `div` 60)

  -- Quit if Escape is pressed, otherwise continue
  M.unless (keyDown SDL.Escape keysDown') $ do
    render screen keysDown'

----------------------------------------------------------------------
    
-- Use pollEvent to return the topmost event from the queue. If there
-- are no events in the queue, pollEvenet will return Nothing.
parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keysDown = do
  maybe_event <- SDL.pollEvent
  case maybe_event of

    -- Nothing happened
    Nothing -> return keysDown

    -- Something happened
    (Just event) ->
      case SDL.eventData event of
        
        -- add a key to the set
        SDL.Keyboard SDL.KeyDown _ _ k -> parseEvents (insert k keysDown)

        -- remove a key from the set
        SDL.Keyboard SDL.KeyUp _ _ k -> parseEvents (delete k keysDown)

        -- add Escape to the set on a Quit event to quit the game
        SDL.Quit -> do
          keyQuit <- SDLK.getKeyFromScancode SDLK.ScancodeEscape
          return (insert (SDL.Keysym SDL.Escape keyQuit 0) keysDown)

        -- otherwise, continue
        _ -> parseEvents keysDown

keyDown :: SDL.Scancode -> Set SDL.Keysym -> Bool
keyDown k = not . null . filter ((== k) . SDL.keyScancode)

-- must make SDL.Keysym an instance of Ord to use it in a Set
deriving instance Ord SDL.Keysym
