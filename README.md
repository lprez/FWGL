FWGL
====

Examples
--------

[Demo](http://ziocroc.github.io/FWGL/demo)

[Recursive signals](http://ziocroc.github.io/FWGL/recur)

[Avoid the obstacles](http://ziocroc.github.io/FWGL/avoid)

[Distortion](http://ziocroc.github.io/FWGL/distortion)

[I/O](https://github.com/ziocroc/FWGL/tree/master/examples/io) (GLFW only)

Installation
------------

        # GLFW:
        cabal install fwgl fwgl-glfw

        # JavaScript:
        cabal install --ghcjs fwgl fwgl-javascript

Documentation
-------------

https://hackage.haskell.org/package/fwgl/docs/FWGL.html

Patch for ghcjs/shims#13
------------------------

If you want to use the GHCJS backend, you need to apply
[fix.patch](https://github.com/ziocroc/FWGL/tree/master/fix.patch)
in ~/.ghcjs/CURRENT_VERSION/ghcjs/shims/src.

Project
--------

### To do

  * Separate packages (fwgl-core, fwgl-yampa, fwgl-javascript, ecc.)
  * Window/canvas customization
  * Sharing, conditionals and loops in the shaders
  * 3D picking
  * Audio
  * AJAX/Local file OBJ loading
  * Lots of examples

### Maybe

  * Skeletal animations
  * Lighting
  * Shadow mapping
  * Bump mapping
  * Network (WebSocket)
  * Android backend
  * Physics
