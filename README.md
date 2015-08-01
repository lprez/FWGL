FWGL
====

![FWGL](http://i.imgur.com/kWUvBCE.png)

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

**[Shadow mapping](http://ziocroc.github.io/FWGL/shadow)**  
Shadow mapping, point light (the example in the screenshot).

**[Demo](http://ziocroc.github.io/FWGL/demo)**  
3D graphics, IO output.

**[Rectangles](http://ziocroc.github.io/FWGL/recur)**  
2D graphics.

**[Distortion](http://ziocroc.github.io/FWGL/distortion)**  
Custom geometry.

**[TCP](https://github.com/ziocroc/FWGL/tree/master/examples/io) (GLFW only)**  
Custom inputs, IO output.


Documentation
-------------

https://hackage.haskell.org/package/fwgl/docs/FWGL.html


Project
-------


**Features that will be implemented:**  

  * Window/canvas customization
  * Inspectable sublayers
  * Separate packages (fwgl-core, fwgl-yampa, ecc.) 
  * Audio

**Features that may be implemented:**  
These can be implemented with the already existing features:
  * 3D picking (with sublayers)
  * Skeletal animations (with the EDSL)
  * Lighting (^)
  * Shadow mapping (^)
  * Bump mapping (^)
  * Network/WebSocket (with custom inputs and outputs)  
 
**Other ideas:**  
  * Android backend
  * Physics
