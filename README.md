FWGL
====

Installing
----------

        # GLFW:
        cabal install fwgl fwgl-glfw

        # JavaScript:
        cabal install --ghcjs fwgl fwgl-javascript
        


#### Patch for ghcjs/shims#13

If you want to use the GHCJS backend, you need to apply
[fix.patch](https://github.com/ziocroc/FWGL/tree/master/fix.patch)
in ~/.ghcjs/CURRENT_VERSION/ghcjs/shims/src.

Examples
--------

**[Avoid the obstacles](http://ziocroc.github.io/FWGL/avoid)**  
A simple 3D game.

**[Demo](http://ziocroc.github.io/FWGL/demo)**  
3D graphics, OBJ loading.

**[Rectangles](http://ziocroc.github.io/FWGL/recur)**  
2D graphics and recursive signals.

**[Distortion](http://ziocroc.github.io/FWGL/distortion)**  
Custom shaders.

**[TCP](https://github.com/ziocroc/FWGL/tree/master/examples/io) (GLFW only)**  
Custom inputs.


Documentation
-------------

https://hackage.haskell.org/package/fwgl/docs/FWGL.html


Project
-------


**Features that will be implemented:**  

  * Window/canvas customization
  * Inspectable sublayers
  * Separate packages (fwgl-core, fwgl-yampa, fwgl-javascript, ecc.) 
  * Audio
  * Lots of examples

**Features that may be implemented:**  
These can be implemented with the already existing features:
  * 3D picking (with sublayers)
  * Skeletal animations (with the EDSL)
  * Lighting (^)
  * Shadow mapping (^)
  * Bump mapping (^)
  * Network (WebSocket) (with custom inputs and outputs)  
 
**Other ideas:**  
  * Android backend
  * Physics
