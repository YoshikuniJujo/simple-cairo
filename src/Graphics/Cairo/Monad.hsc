{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Monad where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Data.Word
import Data.Vector.Storable
import Graphics.Cairo.Types

import Control.Monad.Primitive
import GHC.Base

import Data.CairoContext

#include <cairo.h>

unPrimIo :: PrimMonad m => IO a -> m a
unPrimIo = primitive . unsafeCoerce## . unIO

primIo :: PrimBase m => m a -> IO a
primIo = IO . unsafeCoerce## . internal

-- returnCairoT :: PrimMonad m => IO (Ptr (CairoT (PrimState m))) -> m (CairoT (PrimState m))
-- returnCairoT io = unPrimIo $ makeCairoT =<< io

argCairoT :: PrimMonad m => (Ptr (CairoT (PrimState m)) -> IO a) -> CairoT (PrimState m) -> m a
argCairoT io (CairoT fcr) = unPrimIo $ withForeignPtr fcr io

returnCairoSurfaceT :: PrimMonad m => IO (Ptr (CairoSurfaceT (PrimState m))) -> m (CairoSurfaceT (PrimState m))
returnCairoSurfaceT io = unPrimIo $ makeCairoSurfaceT =<< io

returnCairoSurfaceT' :: PrimMonad m => IO (Ptr (CairoSurfaceT (PrimState m)), Ptr a) -> m (CairoSurfaceT (PrimState m))
returnCairoSurfaceT' io = unPrimIo do
	(ps, p) <- io
	makeCairoSurfaceT' ps p

argCairoSurfaceT :: PrimMonad m => (Ptr (CairoSurfaceT (PrimState m)) -> IO a) -> CairoSurfaceT (PrimState m) -> m a
argCairoSurfaceT io (CairoSurfaceT fs) = unPrimIo $ withForeignPtr fs io

returnCairoPatternT :: PrimMonad m => IO (Ptr (CairoPatternT (PrimState m))) -> m (CairoPatternT (PrimState m))
returnCairoPatternT io = unPrimIo $ makeCairoPatternT =<< io

argCairoPatternT :: PrimMonad m => (Ptr (CairoPatternT (PrimState m)) -> IO a) -> CairoPatternT (PrimState m) -> m a
argCairoPatternT io (CairoPatternT fpt) = unPrimIo $ withForeignPtr fpt io

foreign import ccall "cairo_image_surface_get_data" c_cairo_image_surface_get_data ::
	Ptr (CairoSurfaceT s) -> IO (Ptr #{type unsigned char})

cairoImageSurfaceGetData :: PrimMonad m => CairoSurfaceT (PrimState m) -> m (Vector #{type unsigned char})
cairoImageSurfaceGetData = argCairoSurfaceT \ps -> do
	d <- c_cairo_image_surface_get_data ps
	generateM 100 \i -> peek (d `plusPtr` i)
