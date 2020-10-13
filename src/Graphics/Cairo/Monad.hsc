{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
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
	returnCairoT :: IO (Ptr (CairoT s)) -> m (CairoT s)
	argCairoT :: (Ptr (CairoT s) -> IO a) -> CairoT s -> m a
	returnCairoSurfaceT :: IO (Ptr (CairoSurfaceT s)) -> m (CairoSurfaceT s)
	argCairoSurfaceT :: (Ptr (CairoSurfaceT s) -> IO a) -> CairoSurfaceT s -> m a
	returnCairoPatternT :: IO (Ptr (CairoPatternT s)) -> m (CairoPatternT s)
	argCairoPatternT :: (Ptr (CairoPatternT s) -> IO a) -> CairoPatternT s -> m a

instance CairoMonad s IO where
	returnCairoT io = makeCairoT =<< io
	argCairoT io (CairoT fcr) = withForeignPtr fcr io
	returnCairoSurfaceT io = makeCairoSurfaceT =<< io
	argCairoSurfaceT io (CairoSurfaceT fs) = withForeignPtr fs io
	returnCairoPatternT io = makeCairoPatternT =<< io
	argCairoPatternT io (CairoPatternT p) = withForeignPtr p io

instance CairoMonad s (ST s) where
	returnCairoT io = unsafeIOToST $ returnCairoT io
	argCairoT io cr = unsafeIOToST $ argCairoT io cr
	returnCairoSurfaceT io = unsafeIOToST $ returnCairoSurfaceT io
	argCairoSurfaceT io s = unsafeIOToST $ argCairoSurfaceT io s
	returnCairoPatternT io = unsafeIOToST $ returnCairoPatternT io
	argCairoPatternT io p = unsafeIOToST $ argCairoPatternT io p

foreign import ccall "cairo_image_surface_get_data" c_cairo_image_surface_get_data ::
	Ptr (CairoSurfaceT s) -> IO (Ptr #{type unsigned char})

cairoImageSurfaceGetData :: CairoMonad s m => CairoSurfaceT s -> m (Vector #{type unsigned char})
cairoImageSurfaceGetData = argCairoSurfaceT \ps -> do
	d <- c_cairo_image_surface_get_data ps
	generateM 100 \i -> peek (d `plusPtr` i)
