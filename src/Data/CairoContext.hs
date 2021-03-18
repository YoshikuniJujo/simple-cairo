{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.CairoContext where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Primitive

newtype CairoT s = CairoT (ForeignPtr (CairoT s)) deriving Show

argCairoT :: PrimMonad m => (Ptr (CairoT (PrimState m)) -> IO a) -> CairoT (PrimState m) -> m a
argCairoT io (CairoT fcr) = unsafeIOToPrim $ withForeignPtr fcr io
