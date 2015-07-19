{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

{-|
    The main module. You should also import a backend:

        * FWGL.Backend.JavaScript: GHCJS/WebGL backend (contained in fwgl-javascript)
        * FWGL.Backend.GLFW.GL20: GLFW/OpenGL 2.0 backend (contained in fwgl-glfw)


    And a graphics system:

        * "FWGL.Graphics.D2": 2D graphics
        * "FWGL.Graphics.D3": 3D graphics
        * "FWGL.Graphics.Custom": advanced custom graphics


    "FWGL.Shader" contains the EDSL to make custom shaders.
-}
module FWGL (
        module FWGL.Audio,
        module FWGL.Input,
        module FWGL.Utils,
        module FRP.Yampa,
        draw,
        run,
        run',
        Output,
        (.>),
        io,
) where

import Control.Monad.IO.Class
import FWGL.Audio
import FWGL.Backend
import FWGL.Input
import FWGL.Internal.GL (evalGL)
import FWGL.Graphics.Draw
import FWGL.Graphics.Types
import FWGL.Utils
import FRP.Yampa

-- | The general output.
newtype Output = Output { drawOutput :: Draw () }

-- | Compose two 'Output' effects.
(.>) :: Output -> Output -> Output
Output a .> Output b = Output $ a >> b

-- | Draw some layers.
draw :: BackendIO => [Layer] -> Output
draw layers = Output $ mapM_ drawLayer layers

-- | Perform an IO action.
io :: IO () -> Output
io = Output . liftIO

-- | Run a FWGL program.
run :: BackendIO
    => SF (Input ()) Output  -- ^ Main signal
    -> IO ()
run = run' $ return ()

-- | Run a FWGL program, using custom inputs.
run' :: BackendIO
     => IO inp                -- ^ An IO effect generating the custom inputs.
     -> SF (Input inp) Output
     -> IO ()
run' customInput sigf = setup initState loop customInput sigf
        where initState w h = evalGL $ drawInit w h
              loop (Output act) ctx drawState =
                      flip evalGL ctx . flip execDraw drawState $ do
                              drawBegin
                              act
                              drawEnd
