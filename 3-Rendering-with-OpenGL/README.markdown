Part 3: Rendering with OpenGL
=============================

Let's begin with a description of the modern shader pipeline. Previously, OpenGL used a fixed-function pipeline that was easy for beginners, but limited in usefulness. The modern shader pipeline is very powerful, but has a steep learning curve for beginners. It's best to start with a high-level overview of the pipeline, but before we get to that, what *is* a shader?

Shaders
-------

Shaders are programs that run on the GPU. There are several different types of shaders, but for now we will focus on the most basic ones: vertex shaders and fragment shaders.

Vertex Buffer Objects (VBO)
---------------------------

A Vertex Buffer Object (VBO) is a data structure stored in the memory of the GPU. Since the GPU is specialized for floating-point operations, we want to avoid doing any of these calculations on our CPU. The data bus between the GPU and the rest of our system is incredibly slow, so we want to get our data into the GPU as soon as possible and keep it there until we no longer need it.

First we must create an array of vertices. Our goal will be to form a triangle. We want to construct a 2-dimensional object, so we will use vertices with two elements `GL.Vertex2` where each element is a `GL.GLfloat`, which is a type synonym for C floats. OpenGL uses the center of an object as the origin rather than the top-left corner as many other rendering libraries do. This simplifies 3-dimensional calculations, but adds some complexity for 2-dimensional calculations.

```haskell
vertices :: [GL.Vertex2 GL.GLfloat]
vertices = [ GL.Vertex2   0.0   0.5   -- vertex 1
           , GL.Vertex2   0.5 (-0.5)  -- vertex 2
           , GL.Vertex2 (-0.5) -0.5 ] -- vertex 3
```

Then generate a new buffer. This is quite different from the C API where you would make a call to genBuffers with an argument of "1" to return one buffer. Instead, the Haskell bindings generalize this to genObjectName (for one) and genObjectNames (for more than one).

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