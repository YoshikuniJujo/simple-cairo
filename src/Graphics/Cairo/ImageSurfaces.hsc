{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.ImageSurfaces (
	cairoImageSurfaceCreate,
	cairoImageSurfaceCreateForImageRgba8,
	cairoImageSurfaceGetImage,
	cairoImageSurfaceGetFormat,
	cairoImageSurfaceGetStride,
	cairoImageSurfaceGetCairoImage
	) where

import Foreign.Ptr
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Primitive
import Data.Foldable
import Data.Bits
import Data.Word
import Data.Int
import Codec.Picture
import Codec.Picture.Types

import Graphics.Cairo.Monad
import Graphics.Cairo.Types
import Graphics.Cairo.Values

import Graphics.Cairo.CairoImage hiding (Argb32, pixelAt, Image, Pixel)

#include <cairo.h>

foreign import ccall "cairo_image_surface_create" c_cairo_image_surface_create ::
	#{type cairo_format_t} -> #{type int} -> #{type int} -> IO (Ptr (CairoSurfaceT s))

cairoImageSurfaceCreate :: PrimMonad m => CairoFormatT -> #{type int} -> #{type int} -> m (CairoSurfaceT (PrimState m))
cairoImageSurfaceCreate (CairoFormatT f) w h =
	returnCairoSurfaceT $ c_cairo_image_surface_create f w h

foreign import ccall "cairo_image_surface_get_format" c_cairo_image_surface_get_format ::
	Ptr (CairoSurfaceT s) -> IO #type cairo_format_t

cairoImageSurfaceGetFormat :: PrimMonad m => CairoSurfaceT (PrimState m) -> m CairoFormatT
cairoImageSurfaceGetFormat s = CairoFormatT <$> argCairoSurfaceT c_cairo_image_surface_get_format s

foreign import ccall "cairo_image_surface_get_stride" c_cairo_image_surface_get_stride ::
	Ptr (CairoSurfaceT s) -> IO #type int

cairoImageSurfaceGetStride :: PrimMonad m => CairoSurfaceT (PrimState m) -> m #type int
cairoImageSurfaceGetStride = argCairoSurfaceT c_cairo_image_surface_get_stride

foreign import ccall "cairo_image_surface_get_width" c_cairo_image_surface_get_width ::
	Ptr (CairoSurfaceT s) -> IO #type int

foreign import ccall "cairo_image_surface_get_height" c_cairo_image_surface_get_height ::
	Ptr (CairoSurfaceT s) -> IO #type int

-- foreign import ccall "cairo_image_surface_get_data" c_cairo_image_surface_get_data ::
--	Ptr (CairoSurfaceT s) -> IO (Ptr #{type unsigned char})

cairoImageSurfaceGetImage :: PrimMonad m => CairoSurfaceT (PrimState m) -> m DynamicImage
cairoImageSurfaceGetImage = argCairoSurfaceT \sfc -> do
	d <- c_cairo_image_surface_get_data sfc
	f <- c_cairo_image_surface_get_format sfc
	w <- c_cairo_image_surface_get_width sfc
	h <- c_cairo_image_surface_get_height sfc
	s <- c_cairo_image_surface_get_stride sfc
	case f of
		#{const CAIRO_FORMAT_ARGB32} -> ImageRGBA8 <$> formatArgb32ToImageRgba8 w h s d
		_ -> error "yet"

cairoImageSurfaceGetCairoImage :: PrimMonad m => CairoSurfaceT (PrimState m) -> m CairoImage
cairoImageSurfaceGetCairoImage = argCairoSurfaceT \sfc -> do
	d <- c_cairo_image_surface_get_data sfc
	f <- c_cairo_image_surface_get_format sfc
	w <- c_cairo_image_surface_get_width sfc
	h <- c_cairo_image_surface_get_height sfc
	s <- c_cairo_image_surface_get_stride sfc
	p <- mallocBytes . fromIntegral $ s * h
	copyBytes p d . fromIntegral $ s * h
	fd <- newForeignPtr p $ free p
	pure $ CairoImage f w h s fd

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
	r = (w `shiftR` 16 .&. 0xff) * 0xff `div'` a
	g = (w `shiftR` 8 .&. 0xff) * 0xff `div'` a
	b = (w .&. 0xff) * 0xff `div'` a

div' :: Integral n => n -> n -> n
_ `div'` 0 = 0
n `div'` m = n `div` m

generateImageIo :: Pixel px => (Int -> Int -> IO px) -> Int -> Int -> IO (Image px)
generateImageIo f w h = do
	mi <- newMutableImage w h
	for_ [0 .. h - 1] \y -> for_ [0 .. w - 1] \x -> writePixel mi x y =<< f x y
	freezeImage mi

cairoImageSurfaceCreateForImageRgba8 :: PrimMonad m =>
	Image PixelRGBA8 -> m (CairoSurfaceT (PrimState m))
cairoImageSurfaceCreateForImageRgba8 img = returnCairoSurfaceT' do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_ARGB32} w
	d <- mallocBytes . fromIntegral $ s * h
	imageRgba8ToFormatArgb32 w h s img d
	(, d) <$> c_cairo_image_surface_create_for_data d #{const CAIRO_FORMAT_ARGB32} w h s
	where
	w = fromIntegral $ imageWidth img
	h = fromIntegral $ imageWidth img

foreign import ccall "cairo_image_surface_create_for_data" c_cairo_image_surface_create_for_data ::
	Ptr #{type unsigned char} -> #{type cairo_format_t} -> #{type int} -> #{type int} -> #{type int} -> IO (Ptr (CairoSurfaceT s))

foreign import ccall "cairo_format_stride_for_width" c_cairo_format_stride_for_width ::
	#{type cairo_format_t} -> #{type int} -> IO #{type int}

imageRgba8ToFormatArgb32 :: #{type int} -> #{type int} -> #{type int} ->
	Image PixelRGBA8 -> Ptr #{type unsigned char} -> IO ()
imageRgba8ToFormatArgb32 (fromIntegral -> w) (fromIntegral -> h) (fromIntegral -> s) img (castPtr -> p) =
	for_ [0 .. h - 1] \y -> for_ [0 .. w - 1] \x -> poke (calcPtr 4 s p x y) (rgba8ToArgb32 $ pixelAt img x y)

rgba8ToArgb32 :: PixelRGBA8 -> Argb32
rgba8ToArgb32 (PixelRGBA8 (fromIntegral -> r_) (fromIntegral -> g_) (fromIntegral -> b_) (fromIntegral -> a)) =
	Argb32 $ a `shiftL` 24 .|. r `shiftL` 16 .|.  g `shiftL` 8 .|. b
	where
	r = r_ * a `div` 0xff
	g = g_ * a `div` 0xff
	b = b_ * a `div` 0xff
