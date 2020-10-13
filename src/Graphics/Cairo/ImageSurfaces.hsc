{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.ImageSurfaces (
	cairoImageSurfaceCreate,
	cairoImageSurfaceGetFormat,
	cairoImageSurfaceGetStride
	) where

import Foreign.Ptr
import Data.Bits
import Data.Word
import Data.Int
import Codec.Picture

import Graphics.Cairo.Monad
import Graphics.Cairo.Types
import Graphics.Cairo.Values

#include <cairo.h>

foreign import ccall "cairo_image_surface_create" c_cairo_image_surface_create ::
	#{type cairo_format_t} -> #{type int} -> #{type int} -> IO (Ptr (CairoSurfaceT s))

cairoImageSurfaceCreate :: CairoMonad s m => CairoFormatT -> #{type int} -> #{type int} -> m (CairoSurfaceT s)
cairoImageSurfaceCreate (CairoFormatT f) w h =
	returnCairoSurfaceT $ c_cairo_image_surface_create f w h

foreign import ccall "cairo_image_surface_get_format" c_cairo_image_surface_get_format ::
	Ptr (CairoSurfaceT s) -> IO #type cairo_format_t

cairoImageSurfaceGetFormat :: CairoMonad s m => CairoSurfaceT s -> m CairoFormatT
cairoImageSurfaceGetFormat s = CairoFormatT <$> argCairoSurfaceT c_cairo_image_surface_get_format s

foreign import ccall "cairo_image_surface_get_stride" c_cairo_image_surface_get_stride ::
	Ptr (CairoSurfaceT s) -> IO #type int

cairoImageSurfaceGetStride :: CairoMonad s m => CairoSurfaceT s -> m #type int
cairoImageSurfaceGetStride = argCairoSurfaceT c_cairo_image_surface_get_stride

newtype Argb32 = Argb32 Word32 deriving Show

argb32ToRgba8 :: Argb32 -> PixelRGBA8
argb32ToRgba8 (Argb32 w) = PixelRGBA8 r g b a
	where
	a = fromIntegral $ w `shiftR` 24
	r = fromIntegral $ w `shiftR` 16 .&. 0xff
	g = fromIntegral $ w `shiftR` 8 .&. 0xff
	b = fromIntegral $ w .&. 0xff
