{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.CairoImage.Private (primIo, unPrimIo) where

import GHC.Base
import Control.Monad.Primitive

unPrimIo :: PrimMonad m => IO a -> m a
unPrimIo = primitive . unsafeCoerce## . unIO

primIo :: PrimBase m => m a -> IO a
primIo = IO . unsafeCoerce## . internal
