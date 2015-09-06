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

**[3D picking](http://ziocroc.github.io/FWGL/picking)**  
Render layers.

**[Multiple windows](http://ziocroc.github.io/FWGL/multi)**  

**[TCP](https://github.com/ziocroc/FWGL/tree/master/examples/io) (GLFW only)**  
Custom inputs, IO output.


Documentation
-------------

https://hackage.haskell.org/package/fwgl/docs/FWGL.html


Project
-------


**Features that will be implemented soon:**  
  * Fullscreen windows
  * Audio
  * HTTP requests
  * Network/WebSocket
  * Local storage
  * Blending, culling, etc.

**Other ideas:**  
  * Android backend
  * Partial shaders

Note that FWGL won't include anything that can be already done with the shader EDSL. For this reason, these features are unlikely to be implemented unless I find a decent way to implement partial shaders (I will provide examples anyway):  
  * Lighting
  * Shadow mapping
  * Bump mapping
  * Skyboxes
  * 3D picking
  * Skeletal animations (support for animated model formats may be added).

Also, FWGL is not meant to be a complete game engine, but just a rendering engine with canvas/window management. The name "FWGL" is actually both a pun on GLFW, and the acronym for Functional WebGL. These features will never be added:  
  * Collisions
  * Physics
  * Fonts
