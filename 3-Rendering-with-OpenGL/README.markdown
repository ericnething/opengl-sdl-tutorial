Part 3: Rendering with OpenGL
=============================

Let's begin with a description of the modern shader pipeline. Previously, OpenGL used a fixed-function pipeline that was easy for beginners, but limited in usefulness. The modern shader pipeline is very powerful, but has a steep learning curve for beginners. It's best to start with a high-level overview of the pipeline, but before we get to that, what *is* a shader?

Shaders
-------

Shaders are programs that run on the GPU. There are several different types of shaders, but for now we will focus on the most basic ones: vertex shaders and fragment shaders.

Vertex Buffer Objects (VBO)
---------------------------

First we must create an array of vertices. Our goal will be to form a triangle.

```haskell
vertices = [  0.0,  0.5   -- vertex 1
           ,  0.5, -0.5   -- vertex 2
           , -0.5, -0.5 ] -- vertex 3
```

Then generate a new buffer. This is quite different from the C API where you would make a call to genBuffers with an argument of "1" to return one buffer. Instead, the Haskell bindings generalize this to genObjectName (for one) and genObjectNames (for more than one).

```haskell
vbo <- genObjectName
```

Set this buffer as the active buffer. Here we are using the OpenGL assignment combinator `($=)`. It can be read as "assign". It is used to set values in the GPU's memory. There is also a `get` function used to retrieve data from the GPU. For data that has type GettableStateVar, you can read data using `get`. For data that has type SettableStateVar, you can set data using `($=)`. Data of type StateVar allows both operations.

```haskell
bindBuffer ArrayBuffer $= Just vbo
```

Finally, copy the vertex data into the buffer.

```haskell
bufferData ArrayBuffer $= (sizeOf vertices, vertices, StaticDraw)
```