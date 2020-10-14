{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Monad where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Word
import Data.Vector.Storable
import Graphics.Cairo.Types

#include <cairo.h>

class Monad m => CairoMonad s m where
	ioToCairoMonad :: IO a -> m a

returnCairoT :: forall s m . CairoMonad s m => IO (Ptr (CairoT s)) -> m (CairoT s)
returnCairoT io = ioToCairoMonad @s $ makeCairoT =<< io

argCairoT :: forall s m a . CairoMonad s m => (Ptr (CairoT s) -> IO a) -> CairoT s -> m a
argCairoT io (CairoT fcr) = ioToCairoMonad @s $ withForeignPtr fcr io

returnCairoSurfaceT :: forall s m . CairoMonad s m => IO (Ptr (CairoSurfaceT s)) -> m (CairoSurfaceT s)
returnCairoSurfaceT io = ioToCairoMonad @s $ makeCairoSurfaceT =<< io

returnCairoSurfaceT' :: forall s m a . CairoMonad s m => IO (Ptr (CairoSurfaceT s), Ptr a) -> m (CairoSurfaceT s)
returnCairoSurfaceT' io = ioToCairoMonad @s do
	(ps, p) <- io
	makeCairoSurfaceT' ps p

argCairoSurfaceT :: forall s m a . CairoMonad s m => (Ptr (CairoSurfaceT s) -> IO a) -> CairoSurfaceT s -> m a
argCairoSurfaceT io (CairoSurfaceT fs) = ioToCairoMonad @s $ withForeignPtr fs io

returnCairoPatternT :: forall s m . CairoMonad s m => IO (Ptr (CairoPatternT s)) -> m (CairoPatternT s)
returnCairoPatternT io = ioToCairoMonad @s $ makeCairoPatternT =<< io

argCairoPatternT :: forall s m a . CairoMonad s m => (Ptr (CairoPatternT s) -> IO a) -> CairoPatternT s -> m a
argCairoPatternT io (CairoPatternT fpt) = ioToCairoMonad @s $ withForeignPtr fpt io

instance CairoMonad s IO where ioToCairoMonad = id

instance CairoMonad s (ST s) where ioToCairoMonad = unsafeIOToST

foreign import ccall "cairo_image_surface_get_data" c_cairo_image_surface_get_data ::
	Ptr (CairoSurfaceT s) -> IO (Ptr #{type unsigned char})

cairoImageSurfaceGetData :: CairoMonad s m => CairoSurfaceT s -> m (Vector #{type unsigned char})
cairoImageSurfaceGetData = argCairoSurfaceT \ps -> do
	d <- c_cairo_image_surface_get_data ps
	generateM 100 \i -> peek (d `plusPtr` i)
