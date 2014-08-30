module FWGL.Internal (
        Input
) where

import JavaScript.Event

type Input = [(Event, EventData)]
