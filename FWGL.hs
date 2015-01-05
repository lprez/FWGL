{-|
    The main module. You should also import a backend:

        * "FWGL.Backend.JavaScript": GHCJS/WebGL backend
        * FWGL.Backend.GLFW.GLES20: GLFW/OpenGL ES 2.0 backend (WIP)
        * FWGL.Backend.GLFW.GL32: GLFW/OpenGL 3.2 backend (WIP)


    And a graphics system:

        * "FWGL.Graphics.D2": 2D graphics
        * FWGL.Graphics.D3: 3D graphics (WIP)
        * "FWGL.Graphics.Custom": advanced custom graphics
-}
module FWGL (
        module FWGL.Audio,
        module FWGL.Input,
        module FRP.Yampa,
        Output(..),
        run
) where

import FWGL.Audio
import FWGL.Backend
import FWGL.Input
import FWGL.Internal.GL (evalGL)
import FWGL.Graphics.Draw
import FWGL.Graphics.Types
import FRP.Yampa

-- | The general output.
data Output = Output [Layer] Audio -- StateT ... IO

-- | Run a FWGL program.
run :: BackendIO
    => SF Input Output  -- ^ Main signal
    -> IO ()
run sigf = setup initState loop sigf
        where initState w h = evalGL $ drawInit w h
              loop (Output scenes _) ctx drawState =
                      flip evalGL ctx . flip execDraw drawState $
                              do drawBegin
                                 mapM_ drawLayer scenes
