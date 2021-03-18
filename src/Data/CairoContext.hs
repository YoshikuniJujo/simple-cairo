{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.CairoContext where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Primitive

newtype CairoT s = CairoT (ForeignPtr (CairoT s)) deriving Show

withCairoT :: PrimMonad m => CairoT (PrimState m) -> (Ptr (CairoT (PrimState m)) -> IO a) -> m a
withCairoT (CairoT fcr) = unsafeIOToPrim . withForeignPtr fcr
