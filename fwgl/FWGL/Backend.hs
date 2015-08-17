{-# LANGUAGE ConstraintKinds #-}
module FWGL.Backend (
        module FWGL.Backend.GLES,
        module FWGL.Backend.IO,
        Backend
) where

import FWGL.Backend.GLES
import FWGL.Backend.IO

type Backend = (GLES, BackendIO)
