> module Shaders
>   ( ShaderInfo(..)
>   , loadShaders
>   ) where

> import Control.Exception
> import System.IO
> import System.Exit (exitFailure)
> import qualified Control.Monad             as M
> import qualified Data.ByteString           as BS
> import qualified Graphics.Rendering.OpenGL as GL
> import           Graphics.Rendering.OpenGL (($=))

----------------------------------------------------------------------

Encapsulate the necessary data for loading the shaders into a program.

> data ShaderInfo = ShaderInfo GL.ShaderType FilePath
>                 deriving ( Eq, Ord, Show )

----------------------------------------------------------------------

Create a new program with the specified shaders.

> loadShaders :: [ShaderInfo] -> IO GL.Program
> loadShaders info = do

Create a new program object

>   program <- GL.createProgram

Load, compile, and attach the shaders

>   loadCompileAttach program info

Link and check the program

>   linkAndCheck program

>   return program

----------------------------------------------------------------------

Link the program and check for errors.

> linkAndCheck :: GL.Program -> IO ()
> linkAndCheck program = do

Link the program

>   GL.linkProgram program

Check link status for the program

>   linkOK <- GL.get $ GL.linkStatus program

Validate the program

>   GL.validateProgram program
>   status <- GL.get $ GL.validateStatus program
>   M.unless (linkOK && status) $ do
>     hPutStrLn stderr "GL.linkProgram error"
>     plog <- GL.get $ GL.programInfoLog program
>     putStrLn plog
>     GL.deleteObjectName program
>     exitFailure

----------------------------------------------------------------------

Process the shaders and attach them to the program.

> loadCompileAttach :: GL.Program -> [ShaderInfo] -> IO ()
> loadCompileAttach _ [] = return ()
> loadCompileAttach program (ShaderInfo shaderType source : info) = do

Read the source file into memory

>   src <- BS.readFile source

Create a new shader object

>   shader <- GL.createShader shaderType

Set the source for the shader

>   GL.shaderSourceBS shader $= src

Compile the shader

>   GL.compileShader shader

Check if compilation was successful

>   statusOK <- GL.get $ GL.compileStatus shader
>   M.unless statusOK $ do
>     let shaderType' = case shaderType of
>           GL.VertexShader   -> "vertex"
>           GL.FragmentShader -> "fragment"
>           _                 -> "a"
>     hPutStrLn stderr $ "Error in " ++ shaderType' ++ " shader\n"
>     GL.deleteObjectName shader
>     exitFailure

Attach the shader

>   GL.attachShader program shader

Loop this function

>   loadCompileAttach program info
