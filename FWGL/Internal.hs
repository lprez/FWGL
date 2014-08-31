module FWGL.Internal (
        Input
) where

import Data.HashMap.Strict (HashMap)
import JavaScript.Event (Event, EventData)

type Input = HashMap Event [EventData]
