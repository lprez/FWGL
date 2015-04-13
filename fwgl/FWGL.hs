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
        io,
        (.>),
        run,
        run',
        Output(drawOutput),
        OutputComp,
        X
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
newtype Output a = Output { drawOutput :: Draw () }

-- | This indicates an 'Output' that contains a Draw action.
data X

-- | Compose two 'Output' effects.
(.>) :: OutputComp a b c => Output a -> Output b -> Output c
Output a .> Output b = Output $ a >> b

-- | Composable 'Output's.
class OutputComp a b c | a b -> c where
instance OutputComp () () ()
instance OutputComp () X X
instance OutputComp X () X

-- | Draw some layers.
draw :: BackendIO => [Layer] -> Output X
draw layers = Output $ do drawBegin
                          mapM_ drawLayer layers
                          drawEnd

-- | Perform an IO action.
io :: IO () -> Output ()
io = Output . liftIO

-- | Run a FWGL program.
run :: BackendIO
    => SF (Input ()) (Output a)  -- ^ Main signal
    -> IO ()
run = run' $ return ()

-- | Run a FWGL program, using custom inputs.
run' :: BackendIO
     => IO inp
     -> SF (Input inp) (Output a)
     -> IO ()
run' customInput sigf = setup initState loop customInput sigf
        where initState w h = evalGL $ drawInit w h
              loop (Output act) ctx drawState =
                      flip evalGL ctx . flip execDraw drawState $ act
