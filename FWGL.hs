module FWGL (
        module FWGL.Audio,
        module FWGL.Input,
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

data Output = Output [Layer] Audio -- StateT ... IO

run :: BackendIO        -- ^ Just import FWGL.Backend.JavaScript
    => SF Input Output  -- ^ Main signal
    -> IO ()
run sigf = setup initState loop sigf
        where initState w h = evalGL $ drawInit w h
              loop (Output scenes _) ctx drawState =
                      flip evalGL ctx . flip execDraw drawState $
                              do drawBegin
                                 mapM_ drawLayer scenes
