Part 3: Rendering with OpenGL
=============================

Let's begin with a description of the modern shader pipeline. Previously, OpenGL used a fixed-function pipeline that was easy for beginners, but limited in usefulness. The modern shader pipeline is very powerful, but has a steep learning curve for beginners. It's best to start with a high-level overview of the pipeline, but before we get to that, what *is* a shader?

Shaders
-------

Shaders are programs that run on the GPU. There are several different types of shaders, but for now we will focus on the most basic ones: vertex shaders and fragment shaders.

To build a shader:
1. Create a shader object
2. Load the GLSL source file into the shader object
3. Compile the shader

First, let's build a vertex shader. We create a shader object using `createShader`, passing in the type of shader we want.

```haskell
shader <- GL.createShader GL.VertexShader
```

We need to read our source file into memory using the `readFile` utility from Data.ByteString. Then we can assign it as the source for our shader using `shaderSourceBS`, passing in the shader we want to modify and using the assignment combinator to set the source.

```haskell
src <- BS.readFile vsSourceFile
GL.shaderSourceBS shader $= src
```

Now we can compile the shader using `compileShader` and check if compilation was successful using `compileStatus`.

```haskell
GL.compileShader shader
statusOK <- GL.get (GL.compileStatus shader)
```

We can do the same for our fragment shader.

```haskell
shader <- GL.createShader GL.FragmentShader
src <- BS.readFile fsSourceFile
GL.shaderSourceBS shader $= src
GL.compileShader shader
statusOK <- GL.get (GL.compileStatus shader)
```

Next, we can create a program to hold all of our shaders.

To combine several shaders into a program:
1. Create a program object
2. Attach the compiled shaders
3. Map the fragment shader outputs to buffers in GPU memory
4. Link and check the program
5. Set the program as the active program to use it

To create the program object, use `createProgram`.

```haskell
program <- GL.createProgram
```

Each shader must be attached individuallly using `attachShader`.

```haskell
GL.attachShader program shader
```

To map the outputs of our fragment shader to a buffer in the GPU, we use `bindFragDataLocation`, passing in the program to use, the name of the output, and the buffer to use.

```haskell
GL.bindFragDataLocation program "color" $= 0
```

Link and check the program.

```haskell
GL.linkProgram program
linkOK <- GL.get (GL.linkStatus program)
```

Then set this program as the active program so we can use it. In the C API, this is `glUseProgram`, but the Haskell bindings use `currentProgram`, which accepts a Maybe Program.

```haskell
GL.currentProgram $= Just program
```

Getting Data Into Our Shaders
-----------------------------

Our shaders are programs that accept data as input, but how do we pass the data into them?

To link vertex data with shader attributes:
1. Obtain a reference to the desired attribute in the shader
2. Specify how the data is retrieved from the vertex attribute array
3. Enable the vertex attribute array

We can obtain a reference to our attribute using `AttribLocation` and passing the integral index of that attribute. Attributes in shaders are automatically assigned an index starting at 0 from top to bottom, but they can be explicity specified using `location=` in your shader source file.

To specify how the data is retrieved we use `vertexAttribPointer`. It accepts the attribute location as the first argument, then the second argument is a pair of IntegerHandling and VertexArrayDescriptor wrapped in a StateVar. We want integeral values to be converted to Floats, so we use `ToFloat`. The VertexArrayDescriptor has four components: the dimension of the vertices, the data type, the stride (number of elements to skip between vertices), and a Pointer that specifies the offset from the beginning of the array. We are using a custom utility function `bufferOffset` to create Pointers given an Integral value.

```haskell
GL.vertexAttribPointer (GL.AttribLocation 0) $=

  (GL.ToFloat,       -- integer handling

   GL.VertexArrayDescriptor
   2                 -- dimension of each vertex (length of vector)
   GL.Float          -- data type
   0                 -- stride between elements
   (bufferOffset 0)) -- offset from beginning of array
```

Now we just need to enable the vertex attribute array.

```haskell
GL.vertexAttribArray (AttribLocation 0) $= Enabled
```

Vertex Buffer Object (VBO)
--------------------------

A Vertex Buffer Object (VBO) is a data structure stored in the memory of the GPU. Since the GPU is specialized for floating-point operations, we want to avoid doing any of these calculations on our CPU. The data bus between the GPU and the rest of our system is incredibly slow, so we want to get our data into the GPU as soon as possible and keep it there until we no longer need it.

First we must create an array of vertices. Our goal will be to form a triangle. We want to construct a 2-dimensional object, so we will use vertices with two elements `GL.Vertex2` where each element is a `GL.GLfloat`, which is a type synonym for C floats. OpenGL uses the center of an object as the origin rather than the top-left corner as many other rendering libraries do. This simplifies 3-dimensional calculations, but adds some complexity for 2-dimensional calculations.

```haskell
vertices :: [GL.Vertex2 GL.GLfloat]
vertices = [ GL.Vertex2   0.0   0.5   -- vertex 1
           , GL.Vertex2   0.5 (-0.5)  -- vertex 2
           , GL.Vertex2 (-0.5) -0.5 ] -- vertex 3
```

Then generate a new buffer. This is quite different from the C API where you would make a call to `glGenBuffers` with an argument of "1" to return one buffer. Instead, the Haskell bindings use genObjectName (for one) and genObjectNames (for more than one).

```haskell
vbo <- GL.genObjectName
```

Set this buffer as the active buffer. Here we are using the OpenGL assignment combinator `($=)`. It can be read as "assign". It is used to set values in the GPU's memory. There is also a `get` function used to retrieve data from the GPU. For data that has type GettableStateVar, you can read data using `get`. For data that has type SettableStateVar, you can set data using `($=)`. Data of type StateVar allows both operations.

```haskell
GL.bindBuffer GL.ArrayBuffer $= Just vbo
```

Finally, copy the vertex data into the buffer. We have to do some extra work here to convert the reference to our vertices data into a C pointer using `withArray` from Foreign.Marshal.Array, which creates a temporary C array in memory and generates a pointer to it. Here is the type of `bufferData`.

```haskell
bufferData :: BufferTarget -> StateVar (GLsizeiptr, Ptr a, BufferUsage)
```

We also need to use `sizeOf` from Foreign.Storable to retrieve the size of the element (e.g. Vertex2 GLfloat).

```haskell
let numVertices = fromIntegral $ length vertices
    size        = fromIntegral $ numVertices * sizeOf (head vertices)
```

`withArray` takes a list and a function that accepts a pointer. It converts this list into a C array, stores it in temporary memory, and passes a pointer to that data into the function given in the second argument. Here, `GL.StaticDraw` tells the GPU that this data is static, so it's better to keep it in memory rather than discarding it immediately after rendering.

```haskell
withArray vertices $ \ptr ->
  GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
```

Vertex Array Object (VAO)
-------------------------

A VAO stores references to VBOs and the links between shader inputs and attribute data that is created using `vertexAttribPointer`.

To use a VAO:
1. Create a buffer object
2. Set it as the active VAO

```haskell
vao <- GL.genObjectName
GL.bindVertexArrayObject $= Just vao
```

After creating and enabling a VAO, all vertex attribute array data will be stored in that VAO when using `vertexAttribPointer`.

Drawing to the Screen
---------------------

When drawing to the screen, the current VAO is used. A single call to `drawArrays` is all we need now! It accepts the type of structure to draw (in our case, regular triangles), the vertex to start drawing from, and the total number of vertices to draw. We should also clear the buffer before drawing. Let's use white as the clear color.

```haskell
GL.clearColor $= GL.Color4 1 1 1 1
GL.clear [GL.ColorBuffer]
GL.drawArrays GL.Triangles firstIndex numVertices
```

We can put this into a `draw` function so that it can be called several times. After each call, we want to swap the current screen buffer with the new one, otherwise we won't be able to see any changes on the screen when we add animations. `glSwapWindow` accepts our window handle from when we created the window.

```haskell
SDL.glSwapWindow window
```