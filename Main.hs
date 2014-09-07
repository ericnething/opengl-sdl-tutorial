-- The StandAloneDeriving compiler directive is necessary to create
-- an orphan instance of Ordfor SDL.Keysym
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Prelude hiding (null, filter)
import qualified Control.Monad as M
import Data.Set (Set, empty, insert, delete, null, filter)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Keyboard as SDLK
import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=))

import qualified Data.ByteString as BS
import System.Exit (exitFailure)
import System.IO

import Foreign.Ptr -- for: Ptr, nullPtr, plusPtr
import Foreign.Storable -- for: sizeOf
import Foreign.Marshal.Array -- for: withArray

import Shaders
----------------------------------------------------------------------

winWidth = 640
winHeight = 480

----------------------------------------------------------------------

main :: IO ()
main = do
  
  -- Set constants for use in window creation
  let title     = "Eric's SDL and OpenGL Tutorial"
      position  = SDL.Position 0 0
      size      = SDL.Size winWidth winHeight
      win_flags = [SDL.WindowOpengl]

  -- Create the window
  window <- SDL.createWindow title position size win_flags

  -- Initialize OpenGL
  initGL

  -- Set up a rendering context
  renderer <- SDL.glCreateContext window

  -- Synchronize the buffer swap with the display's vertical refresh rate
  -- SDL.glSetSwapInterval SDL.SynchronizedUpdates

  descriptor <- initResources

  -- Run main loop
  mainLoop window empty descriptor

  -- Destroy the rendering context
  SDL.glDeleteContext renderer

  -- Destroy the window
  SDL.destroyWindow window

----------------------------------------------------------------------

-- | Set OpenGL context attributes
initGL :: IO ()
initGL = do
  
      -- for convenience
  let setAttr = SDL.glSetAttribute
      -- convert the Profile value to a CInt
      profileVersion = SDL.sdlGLAttributeToC SDL.GLContextProfileCore
      
  -- OpenGL profile and version
  setAttr SDL.GLContextProfileMask profileVersion
  setAttr SDL.GLContextMajorVersion 3
  setAttr SDL.GLContextMinorVersion 2

  -- Enable double buffering with a 24 bit Z buffer
  setAttr SDL.GLDoubleBuffer 1
  setAttr SDL.GLDepthSize 24

data Descriptor = Descriptor GL.VertexArrayObject GL.ArrayIndex GL.NumArrayIndices

initResources :: IO Descriptor
initResources = do

-- 1. Create the VAO
  
  -- create a Vertex Array Object (VAO)
  triangle <- GL.genObjectName

  -- set it as the active VAO
  GL.bindVertexArrayObject $= Just triangle

-- 2. Create the VBO

  -- create a Vertex Buffer Object (VBO)
  vbo <- GL.genObjectName

  -- set it as the active VBO
  GL.bindBuffer GL.ArrayBuffer $= Just vbo

  let numVertices = fromIntegral $ length vertices
      size = fromIntegral $ numVertices * sizeOf (head vertices)

  -- copy the vertex data into the VBO
  withArray vertices $ \ptr ->
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)

-- 3. Load the shaders

  -- load the shaders
  program <- loadShaders [ ShaderInfo GL.VertexShader "triangle.vertex"
                         , ShaderInfo GL.FragmentShader "triangle.fragment" ]

  -- set this program as the active one
  GL.currentProgram $= Just program

-- 4. Link vertex data with attributes
      
  -- obtain a reference to the desired attribute in the shader
  -- and specify how the data wil be retrieved from the array
  GL.vertexAttribPointer (GL.AttribLocation 0) $=
    (GL.ToFloat, GL.VertexArrayDescriptor
                 2                 -- how many dimensions per vertex
                 GL.Float          -- data type
                 0                 -- stride between elements
                 (bufferOffset 0)) -- offset from beginning of array

  -- set this vertex attribute array as the active one
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

  return $ Descriptor triangle 0 (fromIntegral numVertices)

bufferOffset :: Integral a => a -> Ptr a
bufferOffset = plusPtr nullPtr . fromIntegral

draw :: SDL.Window -> Descriptor -> IO ()
draw win (Descriptor triangle firstIndex numVertices) = do

  -- set the clear color
  GL.clearColor $= GL.Color4 1 1 1 1

  -- clear the buffer
  GL.clear [GL.ColorBuffer]

  -- set up the viewport
  winSize <- SDL.getWindowSize win
  GL.viewport $= (GL.Position 0 0,
                  GL.Size (fromIntegral winWidth) (fromIntegral winHeight))

  -- draw to the buffer
  GL.drawArrays GL.TriangleStrip firstIndex numVertices

  -- -- disable vertex array
  -- GL.vertexAttribArray attrib $= GL.Disabled

vertices :: [GL.Vertex2 GL.GLfloat]
vertices = [ GL.Vertex2 (-0.7)   0.4
           , GL.Vertex2   0.1    0.9
           , GL.Vertex2 (-0.3) (-0.4)
           , GL.Vertex2   0.4  (-0.4) ]

-- render :: SDL.Window -> IO ()
render screen = do
  GL.clearColor $= GL.Color4 1.0 1.0 1.0 1.0
  GL.clear [GL.ColorBuffer]

-- | primary rendering action
mainLoop screen keysDown descriptor = do

  -- capture any new events
  keysDown' <- parseEvents keysDown

  -- render the context
--  render screen
  draw screen descriptor

  SDL.glSwapWindow screen
  
  -- Quit if Escape is pressed, otherwise continue
  M.unless (keyDown SDL.Escape keysDown') $ do
    mainLoop screen keysDown' descriptor

----------------------------------------------------------------------

-- -- initialize shaders
-- initShaders :: IO (GL.Program, GL.AttribLocation)
-- initShaders = do
  
--   vs <- getShader GL.VertexShader vsSource
--   fs <- getShader GL.FragmentShader fsSource

--   -- create a new program object
--   program <- GL.createProgram

--   -- attach the compiled shaders
--   GL.attachShader program vs
--   GL.attachShader program fs

--   -- set the attribute location for the program
--   GL.attribLocation program "coord2d" $= GL.AttribLocation 0

--   -- link the program
--   GL.linkProgram program
  
--   -- check link status for the program
--   linkOK <- GL.get $ GL.linkStatus program

--   -- validate the program
--   GL.validateProgram program
--   status <- GL.get $ GL.validateStatus program
--   M.unless (linkOK && status) $ do
--     hPutStrLn stderr "GL.linkProgram error"
--     plog <- GL.get $ GL.programInfoLog program
--     putStrLn plog
--     exitFailure

--   -- set it as the current program
--   GL.currentProgram $= Just program
  
--   return (program, GL.AttribLocation 0)

-- getShader :: GL.ShaderType -> BS.ByteString -> IO GL.Shader
-- getShader shaderType source = do
  
--   -- create a new shader object
--   s <- GL.createShader shaderType

--   -- set the source for the shader
--   GL.shaderSourceBS s $= source

--   -- compile the shader
--   GL.compileShader s

--   -- check if compilation was successful
--   statusOK <- GL.get $ GL.compileStatus s
--   M.unless statusOK $ do
--     let shaderType' = case shaderType of
--           GL.VertexShader   -> "vertex"
--           GL.FragmentShader -> "fragment"
--           _                 -> "a"
--     hPutStrLn stderr $ "Error in " ++ shaderType' ++ " shader\n"
--     exitFailure
    
--   return s


----------------------------------------------------------------------
    
-- | Use pollEvent to return the topmost event from the queue. If there
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
