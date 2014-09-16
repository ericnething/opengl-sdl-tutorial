Part 2: OpenGL Setup
====================

The Haskell OpenGL bindings come with the Haskell Platform, but if you don't have them installed, you can grab them using `cabal install opengl`. Make sure to use the latest version from Hackage.

Initializing OpenGL
-------------------

We will put all of our initialization calls into a function `initGL`.

First, you must set the Profile version. I chose to use the Core Profile for forwards compatibility. Currently, there is not a clean way to set the Profile because `SDL.glSetAttribute` expects a CInt. So I exported a utility function from the SDL bindings to convert the Profile constant, which may be different depending on your platform. I used a let statement to reduce typing and improve clarity.

```haskell
  let setAttr        = SDL.glSetSttribute
      profileVersion = SDL.sdlGLAttributeToC SDL.GLContextProfileCore

  setAttr SDL.GLContextProfileMask profileVersion
  setAttr SDL.GLContextMajorVersion 3
  setAttr SDL.GLContextMinorVersion 2
```

We want to set the Major version to 3 and Minor version to 2, which corresponds to OpenGL version 3.2. We actually want to use version 4.1 (on OSX), but by setting the version to 3.2 and enabling the Core Profile, we are telling SDL to use the highest version of OpenGL available on our system -- which is 4.1.

We also want to enable double buffering so that rendering can happen in the background and then swapped in when the output frame updates. Set the buffer size to 24 bits.

```haskell
setAttr SDL.GLDoubleBuffer 1
setAttr SDL.GLDepthSize 24
```

Creating the Rendering Context
------------------------------

After creating the window and initializing OpenGL, create an OpenGL Context.

```haskell
renderer <- SDL.glCreateContext window
```

As part of the clean-up, before destroying the SDL window, delete the OpenGL Context.

```haskell
SDL.glDeleteContext renderer
```

Run the program. If it opens a blank window, like in the previous tutorial, then it is working properly. We aren't rendering anything yet, but in the next part of the tutorial we will have something show up inside the window.