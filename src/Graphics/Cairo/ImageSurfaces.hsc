{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.ImageSurfaces (
	cairoImageSurfaceCreate
	) where

import Foreign.Ptr
import Data.Int

import Graphics.Cairo.Monad
import Graphics.Cairo.Types
import Graphics.Cairo.Values

#include <cairo.h>

foreign import ccall "cairo_image_surface_create" c_cairo_image_surface_create ::
	#{type cairo_format_t} -> #{type int} -> #{type int} -> IO (Ptr (CairoSurfaceT s))

cairoImageSurfaceCreate :: CairoMonad s m => CairoFormatT -> #{type int} -> #{type int} -> m (CairoSurfaceT s)
cairoImageSurfaceCreate (CairoFormatT f) w h =
	returnCairoSurfaceT $ c_cairo_image_surface_create f w h
