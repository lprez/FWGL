module FWGL (
        module FWGL.Audio,
        module FWGL.Input,
        module FWGL.Graphics,
        Output(..),
        run
) where

import Debug.Trace
import FWGL.Audio
import FWGL.Backend
import FWGL.Input
import FWGL.Internal.GL (evalGL)
import FWGL.Graphics
import FWGL.Graphics.Draw
import FRP.Yampa

data Output = Output [Scene] Audio

run :: BackendIO        -- ^ Just import FWGL.Backend.JavaScript
    => SF Input Output  -- ^ Main signal
    -> IO ()
run sigf = putStrLn "setup" >> setup initState loop sigf
-- run = setup initState loop
        where initState w h = trace "evalGL drawInit" . evalGL . trace "drawInit" $ drawInit w h
              loop (Output scenes _) ctx drawState =
                      trace "loop" . flip evalGL ctx . flip execDraw drawState $
                              do drawBegin
                                 mapM_ drawScene scenes
