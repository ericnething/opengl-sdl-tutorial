The StandAloneDeriving compiler directive is necessary to create
an orphan instance of Ordfor SDL.Keysym

> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE OverloadedStrings #-}
> 
> module Main
>   ( main
>   ) where

> import Prelude hiding (null, filter)
> import qualified Control.Monad as M
> import Data.Set (Set, empty, insert, delete, null, filter)
> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.Keyboard as SDLK
> import qualified Graphics.Rendering.OpenGL as GL
> import           Graphics.Rendering.OpenGL (($=))

> import qualified Data.ByteString as BS
> import System.Exit (exitFailure)
> import System.IO

> import Foreign.Ptr -- for: Ptr, nullPtr, plusPtr
> import Foreign.Storable -- for: sizeOf
> import Foreign.Marshal.Array -- for: withArray

> import Shaders

----------------------------------------------------------------------

> winWidth = 640
> winHeight = 480

----------------------------------------------------------------------

> main :: IO ()
> main = do

Set constants for use in window creation

>   let title     = "Eric's SDL and OpenGL Tutorial"
>       position  = SDL.Position 0 0
>       size      = SDL.Size winWidth winHeight
>       win_flags = [SDL.WindowOpengl]

Create the window

>   window <- SDL.createWindow title position size win_flags

Initialize OpenGL

>   initGL

Set up a rendering context

>   renderer <- SDL.glCreateContext window

Synchronize the buffer swap with the display's vertical refresh rate

>   -- SDL.glSetSwapInterval SDL.SynchronizedUpdates

Get the descriptor

>   descriptor <- initResources

Run main loop

>   mainLoop window empty descriptor

Destroy the rendering context

>   SDL.glDeleteContext renderer

Destroy the window

>   SDL.destroyWindow window

----------------------------------------------------------------------

Set OpenGL context attributes

> initGL :: IO ()
> initGL = do
>   
>       -- for convenience
>   let setAttr = SDL.glSetAttribute
>       -- convert the Profile value to a CInt
>       profileVersion = SDL.sdlGLAttributeToC SDL.GLContextProfileCore

OpenGL profile and version

>   setAttr SDL.GLContextProfileMask profileVersion
>   setAttr SDL.GLContextMajorVersion 3
>   setAttr SDL.GLContextMinorVersion 2

Enable double buffering with a 24 bit Z buffer

>   setAttr SDL.GLDoubleBuffer 1
>   setAttr SDL.GLDepthSize 24

----------------------------------------------------------------------

Create a Descriptor to contain the information about the VAO

> data Descriptor = Descriptor GL.VertexArrayObject GL.ArrayIndex GL.NumArrayIndices

----------------------------------------------------------------------

> initResources :: IO Descriptor
> initResources = do

1. Create the VAO

Create a Vertex Array Object (VAO)

>   triangle <- GL.genObjectName
> 
>   -- set it as the active VAO
>   GL.bindVertexArrayObject $= Just triangle

2. Create the VBO

Create a Vertex Buffer Object (VBO)

>   vbo <- GL.genObjectName

Set it as the active VBO

>   GL.bindBuffer GL.ArrayBuffer $= Just vbo

For convenience, let's define the number of vertices and size

>   let numVertices = fromIntegral $ length vertices
>       size = fromIntegral $ numVertices * sizeOf (head vertices)

Copy the vertex data into the VBO

>   withArray vertices $ \ptr ->
>     GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)

3. Loading the shaders

Load the shaders

>   program <- loadShaders [ ShaderInfo GL.VertexShader "triangle.vertex"
>                          , ShaderInfo GL.FragmentShader "triangle.fragment" ]

Set this program as the active one

>   GL.currentProgram $= Just program

4. Link vertex data with attributes

Obtain a reference to the desired attribute in the shader and specify
how the data wil be retrieved from the array.

>   GL.vertexAttribPointer (GL.AttribLocation 0) $=
>     (GL.ToFloat, GL.VertexArrayDescriptor
>                  2                 -- how many dimensions per vertex
>                  GL.Float          -- data type
>                  0                 -- stride between elements
>                  (bufferOffset 0)) -- offset from beginning of array

Set this vertex attribute array as the active one

>   GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
> 
>   return $ Descriptor triangle 0 (fromIntegral numVertices)

> bufferOffset :: Integral a => a -> Ptr a
> bufferOffset = plusPtr nullPtr . fromIntegral

> draw :: SDL.Window -> Descriptor -> IO ()
> draw win (Descriptor triangle firstIndex numVertices) = do


Set the clear color

>   GL.clearColor $= GL.Color4 1 1 1 1

Clear the buffer

>   GL.clear [GL.ColorBuffer]
 
Set up the viewport

>   winSize <- SDL.getWindowSize win
>   GL.viewport $= (GL.Position 0 0,
>                   GL.Size (fromIntegral winWidth) (fromIntegral winHeight)) 

Draw to the buffer

>   GL.drawArrays GL.TriangleStrip firstIndex numVertices

Disable vertex array

>   -- GL.vertexAttribArray attrib $= GL.Disabled

----------------------------------------------------------------------

> vertices :: [GL.Vertex2 GL.GLfloat]
> vertices = [ GL.Vertex2 (-0.7)   0.4
>            , GL.Vertex2   0.1    0.9
>            , GL.Vertex2 (-0.3) (-0.4)
>            , GL.Vertex2   0.4  (-0.4) ]

----------------------------------------------------------------------

> -- render :: SDL.Window -> IO ()
> render screen = do
>   GL.clearColor $= GL.Color4 1.0 1.0 1.0 1.0
>   GL.clear [GL.ColorBuffer]

Primary rendering action

> mainLoop screen keysDown descriptor = do 

Capture any new events

>   keysDown' <- parseEvents keysDown

Render the context

>   draw screen descriptor

Swap the buffers

>   SDL.glSwapWindow screen

Quit if Escape is pressed, otherwise continue

>   M.unless (keyDown SDL.Escape keysDown') $ do
>     mainLoop screen keysDown' descriptor

----------------------------------------------------------------------
    
Use pollEvent to return the topmost event from the queue. If there are
no events in the queue, pollEvenet will return Nothing.

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

We must make SDL.Keysym an instance of Ord to use it in a Set

> deriving instance Ord SDL.Keysym
