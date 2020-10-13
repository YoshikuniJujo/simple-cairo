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
import Data.Int
import Data.Vector.Storable
import Graphics.Cairo.Types
import Graphics.Cairo.Values

#include <cairo.h>

class CairoMonad s m where
	returnCairoT :: IO (Ptr (CairoT s)) -> m (CairoT s)
	argCairoT :: (Ptr (CairoT s) -> IO a) -> CairoT s -> m a
	returnCairoSurfaceT :: IO (Ptr (CairoSurfaceT s)) -> m (CairoSurfaceT s)
	argCairoSurfaceT :: (Ptr (CairoSurfaceT s) -> IO a) -> CairoSurfaceT s -> m a

instance CairoMonad s IO where
	returnCairoT io = makeCairoT =<< io
	argCairoT io (CairoT fcr) = withForeignPtr fcr io
	returnCairoSurfaceT io = makeCairoSurfaceT =<< io
	argCairoSurfaceT io (CairoSurfaceT fs) = withForeignPtr fs io

instance CairoMonad s (ST s) where
	returnCairoT io = unsafeIOToST $ returnCairoT io
	argCairoT io cr = unsafeIOToST $ argCairoT io cr
	returnCairoSurfaceT io = unsafeIOToST $ returnCairoSurfaceT io
	argCairoSurfaceT io s = unsafeIOToST $ argCairoSurfaceT io s

foreign import ccall "cairo_create" c_cairo_create :: Ptr (CairoSurfaceT s) -> IO (Ptr (CairoT s))

cairoCreate :: CairoMonad s m => CairoSurfaceT s -> m (CairoT s)
cairoCreate s = returnCairoT $ argCairoSurfaceT c_cairo_create s

foreign import ccall "cairo_image_surface_create" c_cairo_image_surface_create ::
	#{type cairo_format_t} -> #{type int} -> #{type int} -> IO (Ptr (CairoSurfaceT s))

cairoImageSurfaceCreate ::
	CairoMonad s m => CairoFormatT -> #{type int} -> #{type int} -> m (CairoSurfaceT s)
cairoImageSurfaceCreate (CairoFormatT f) w h = returnCairoSurfaceT $ c_cairo_image_surface_create f w h

foreign import ccall "cairo_set_source_rgb" c_cairo_set_source_rgb ::
	Ptr (CairoT s) -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoSetSourceRgb ::
	CairoMonad s m => CairoT s -> #{type double} -> #{type double} -> #{type double} -> m ()
cairoSetSourceRgb cr r g b = argCairoT (\pcr -> c_cairo_set_source_rgb pcr r g b) cr

foreign import ccall "cairo_paint" c_cairo_paint :: Ptr (CairoT s) -> IO ()

cairoPaint :: CairoMonad s m => CairoT s -> m ()
cairoPaint = argCairoT c_cairo_paint

foreign import ccall "cairo_image_surface_get_data" c_cairo_image_surface_get_data ::
	Ptr (CairoSurfaceT s) -> IO (Ptr #{type unsigned char})

cairoImageSurfaceGetData :: CairoMonad s m => CairoSurfaceT s -> m (Vector #{type unsigned char})
cairoImageSurfaceGetData = argCairoSurfaceT \ps -> do
	d <- c_cairo_image_surface_get_data ps
	generateM 100 \i -> peek (d `plusPtr` i)
