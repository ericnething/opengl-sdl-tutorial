module Shaders
  ( ShaderInfo(..)
  , loadShaders
  ) where

import Control.Exception
import System.IO
import System.Exit (exitFailure)
import qualified Control.Monad             as M
import qualified Data.ByteString           as BS
import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=))

----------------------------------------------------------------------

data ShaderInfo = ShaderInfo GL.ShaderType FilePath
                deriving ( Eq, Ord, Show )

loadShaders :: [ShaderInfo] -> IO GL.Program
loadShaders info = do

  -- create a new program object
  program <- GL.createProgram

  -- load, compile, and attach the shaders
  loadCompileAttach program info

  -- link and check the program
  linkAndCheck program

  return program

linkAndCheck :: GL.Program -> IO ()
linkAndCheck program = do

  -- link the program
  GL.linkProgram program
  
  -- check link status for the program
  linkOK <- GL.get $ GL.linkStatus program

  -- validate the program
  GL.validateProgram program
  status <- GL.get $ GL.validateStatus program
  M.unless (linkOK && status) $ do
    hPutStrLn stderr "GL.linkProgram error"
    plog <- GL.get $ GL.programInfoLog program
    putStrLn plog
    GL.deleteObjectName program
    exitFailure

loadCompileAttach :: GL.Program -> [ShaderInfo] -> IO ()
loadCompileAttach _ [] = return ()
loadCompileAttach program (ShaderInfo shaderType source : info) = do

  -- read the source file into memory
  src <- BS.readFile source

  -- create a new shader object
  shader <- GL.createShader shaderType

  -- set the source for the shader
  GL.shaderSourceBS shader $= src

  -- compile the shader
  GL.compileShader shader

  -- check if compilation was successful
  statusOK <- GL.get $ GL.compileStatus shader
  M.unless statusOK $ do
    let shaderType' = case shaderType of
          GL.VertexShader   -> "vertex"
          GL.FragmentShader -> "fragment"
          _                 -> "a"
    hPutStrLn stderr $ "Error in " ++ shaderType' ++ " shader\n"
    GL.deleteObjectName shader
    exitFailure

  -- attach the shader
  GL.attachShader program shader

  loadCompileAttach program info
