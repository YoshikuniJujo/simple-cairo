{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.ImageSurfaces (
	cairoImageSurfaceCreate,
	cairoImageSurfaceGetImage,
	cairoImageSurfaceGetFormat,
	cairoImageSurfaceGetStride
	) where

import Foreign.Ptr
import Foreign.Storable
import Data.Foldable
import Data.Bits
import Data.Word
import Data.Int
import Codec.Picture
import Codec.Picture.Types

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

foreign import ccall "cairo_image_surface_get_width" c_cairo_image_surface_get_width ::
	Ptr (CairoSurfaceT s) -> IO #type int

foreign import ccall "cairo_image_surface_get_height" c_cairo_image_surface_get_height ::
	Ptr (CairoSurfaceT s) -> IO #type int

-- foreign import ccall "cairo_image_surface_get_data" c_cairo_image_surface_get_data ::
--	Ptr (CairoSurfaceT s) -> IO (Ptr #{type unsigned char})

cairoImageSurfaceGetImage :: CairoMonad s m => CairoSurfaceT s -> m DynamicImage
cairoImageSurfaceGetImage = argCairoSurfaceT \sfc -> do
	d <- c_cairo_image_surface_get_data sfc
	f <- c_cairo_image_surface_get_format sfc
	w <- c_cairo_image_surface_get_width sfc
	h <- c_cairo_image_surface_get_height sfc
	s <- c_cairo_image_surface_get_stride sfc
	case f of
		#{const CAIRO_FORMAT_ARGB32} -> ImageRGBA8 <$> formatArgb32ToImageRgba8 w h s d
		_ -> error "yet"

formatArgb32ToImageRgba8 :: #{type int} -> #{type int} -> #{type int} ->
	Ptr #{type unsigned char} -> IO (Image PixelRGBA8)
formatArgb32ToImageRgba8 (fromIntegral -> w) (fromIntegral -> h) (fromIntegral -> s) (castPtr -> p) =
	generateImageIo (\x y -> argb32ToRgba8 <$> peekArgb32 4 s p x y) w h

peekArgb32 :: Int -> Int -> Ptr Argb32 -> Int -> Int -> IO Argb32
peekArgb32 byts s p x y = peek $ calcPtr byts s p x y

calcPtr :: Int -> Int -> Ptr a -> Int -> Int -> Ptr a
calcPtr byts s p x y = p `plusPtr` (x * byts + y * s)

newtype Argb32 = Argb32 Word32 deriving (Show, Storable)

argb32ToRgba8 :: Argb32 -> PixelRGBA8
argb32ToRgba8 (Argb32 w) = PixelRGBA8
	(fromIntegral r)
	(fromIntegral g)
	(fromIntegral b)
	(fromIntegral a)
	where
	a = w `shiftR` 24
	r = (w `shiftR` 16 .&. 0xff) * 0xff `div` a
	g = (w `shiftR` 8 .&. 0xff) * 0xff `div` a
	b = (w .&. 0xff) * 0xff `div` a

generateImageIo :: Pixel px => (Int -> Int -> IO px) -> Int -> Int -> IO (Image px)
generateImageIo f w h = do
	mi <- newMutableImage w h
	for_ [0 .. h - 1] \y -> for_ [0 .. w - 1] \x -> writePixel mi x y =<< f x y
	freezeImage mi
