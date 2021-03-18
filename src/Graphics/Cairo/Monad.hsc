{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Monad where

import Foreign.Ptr
import Foreign.ForeignPtr
import Graphics.Cairo.Types

import Control.Monad.Primitive
import GHC.Base

import Data.CairoContext

#include <cairo.h>

unPrimIo :: PrimMonad m => IO a -> m a
unPrimIo = primitive . unsafeCoerce## . unIO

primIo :: PrimBase m => m a -> IO a
primIo = IO . unsafeCoerce## . internal

argCairoT :: PrimMonad m => (Ptr (CairoT (PrimState m)) -> IO a) -> CairoT (PrimState m) -> m a
argCairoT io (CairoT fcr) = unPrimIo $ withForeignPtr fcr io

returnCairoPatternT :: PrimMonad m => IO (Ptr (CairoPatternT (PrimState m))) -> m (CairoPatternT (PrimState m))
returnCairoPatternT io = unPrimIo $ makeCairoPatternT =<< io

argCairoPatternT :: PrimMonad m => (Ptr (CairoPatternT (PrimState m)) -> IO a) -> CairoPatternT (PrimState m) -> m a
argCairoPatternT io (CairoPatternT fpt) = unPrimIo $ withForeignPtr fpt io
