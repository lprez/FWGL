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

**[Demo](http://ziocroc.github.io/FWGL/demo)**  
Various.

**[Recursive signals](http://ziocroc.github.io/FWGL/recur)**  
2D graphics and interaction.

**[Avoid the obstacles](http://ziocroc.github.io/FWGL/avoid)**  
A simple game.

**[Distortion](http://ziocroc.github.io/FWGL/distortion)**  
Demonstrates the use of custom shaders.


**[TCP](https://github.com/ziocroc/FWGL/tree/master/examples/io) (GLFW only)**  
Demonstrates the use of custom inputs and IO effects.


Documentation
-------------

https://hackage.haskell.org/package/fwgl/docs/FWGL.html


Project
-------


**Features that will be implemented:**  

  * Separate packages (fwgl-core, fwgl-yampa, fwgl-javascript, ecc.) 
  * Window/canvas customization
  * Sharing, conditionals and loops in the shaders (WIP on **actions** branch)
  * AJAX/Local file OBJ loading
  * Audio
  * Inspectable sublayers
  * Lots of examples and better documentation

**Features that may be implemented:**  
These can be implemented with the already existing features:
  * 3D picking (with sublayers)
  * Skeletal animations (with the EDSL)
  * Lighting (^)
  * Shadow mapping (^)
  * Bump mapping (^)
  * Network (WebSocket) (with custom inputs and outputs)  
 
**Other:**  
  * Android backend
  * Physics
