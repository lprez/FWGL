FWGL
====

Examples
--------

[Demo](http://ziocroc.github.io/FWGL/demo)

[Recursive signals](http://ziocroc.github.io/FWGL/recur)

[Avoid the obstacles](http://ziocroc.github.io/FWGL/avoid)

[Distortion](http://ziocroc.github.io/FWGL/distortion)

Installation
------------

        # GLFW:
        cabal install fwgl
        cabal install fwgl-glfw

        # JavaScript:
        cabal install --ghcjs fwgl
        cabal install --ghcjs fwgl-javascript

Documentation
-------------

https://hackage.haskell.org/package/fwgl/docs/FWGL.html

Project
--------

### Experimental features

  * Separated backend
  * Custom shaders
  * Shader EDSL
  * Custom graphics system
  * GLFW Backend

### To do

  * Camera
  * Lighting
  * 3D picking
  * Audio/side effects
  * AJAX/Local file OBJ loading
  * Window/canvas customization
  * Lots of examples
  * Skeletal animations

### Maybe

  * Shadow mapping
  * Bump mapping
  * Network (WebSocket) (May be replaced by a general support for Input signal with side effects)
  * Separate packages (fwgl-core, fwgl-yampa, fwgl-javascript, ecc.)
  * Android backend
  * Physics
