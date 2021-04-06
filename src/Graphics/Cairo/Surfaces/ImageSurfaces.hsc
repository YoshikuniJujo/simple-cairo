{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.ImageSurfaces (
	cairoImageSurfaceCreate,

	cairoImageSurfaceCreateForCairoImage,
	cairoImageSurfaceCreateForCairoImageMut,

	cairoImageSurfaceGetCairoImage,
	cairoImageSurfaceGetCairoImageMut,
	) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Primitive
import Data.Word
import Data.Int

import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal
import Graphics.Cairo.Values

import Data.CairoImage.Internal hiding (Argb32, pixelAt, Image, Pixel)

#include <cairo.h>

newtype CairoSurfaceImageT s = CairoSurfaceImageT (ForeignPtr (CairoSurfaceT s)) deriving Show

instance IsCairoSurfaceT CairoSurfaceImageT where
	toCairoSurfaceT (CairoSurfaceImageT fsr) = CairoSurfaceT fsr

cairoImageSurfaceCreate :: PrimMonad m => CairoFormatT -> #{type int} -> #{type int} -> m (CairoSurfaceImageT (PrimState m))
cairoImageSurfaceCreate (CairoFormatT f) w h =
	returnCairoSurfaceT $ c_cairo_image_surface_create f w h

foreign import ccall "cairo_image_surface_create" c_cairo_image_surface_create ::
	#{type cairo_format_t} -> #{type int} -> #{type int} -> IO (Ptr (CairoSurfaceT s))

foreign import ccall "cairo_image_surface_get_format" c_cairo_image_surface_get_format ::
	Ptr (CairoSurfaceT s) -> IO #type cairo_format_t

foreign import ccall "cairo_image_surface_get_stride" c_cairo_image_surface_get_stride ::
	Ptr (CairoSurfaceT s) -> IO #type int

foreign import ccall "cairo_image_surface_get_width" c_cairo_image_surface_get_width ::
	Ptr (CairoSurfaceT s) -> IO #type int

foreign import ccall "cairo_image_surface_get_height" c_cairo_image_surface_get_height ::
	Ptr (CairoSurfaceT s) -> IO #type int

cairoImageSurfaceGetCairoImage :: PrimMonad m => CairoSurfaceImageT (PrimState m) -> m CairoImage
cairoImageSurfaceGetCairoImage = argCairoSurfaceT \sfc -> do
	d <- c_cairo_image_surface_get_data sfc
	f <- c_cairo_image_surface_get_format sfc
	w <- c_cairo_image_surface_get_width sfc
	h <- c_cairo_image_surface_get_height sfc
	s <- c_cairo_image_surface_get_stride sfc
	p <- mallocBytes . fromIntegral $ s * h
	copyBytes p d . fromIntegral $ s * h
	fd <- newForeignPtr (castPtr p) $ free (castPtr p)
	pure $ CairoImage f (fromIntegral w) (fromIntegral h) (fromIntegral s) fd

cairoImageSurfaceGetCairoImageMut :: PrimMonad m => CairoSurfaceImageT (PrimState m) -> m (CairoImageMut (PrimState m))
cairoImageSurfaceGetCairoImageMut = argCairoSurfaceT \sfc -> do
	d <- c_cairo_image_surface_get_data sfc
	f <- c_cairo_image_surface_get_format sfc
	w <- c_cairo_image_surface_get_width sfc
	h <- c_cairo_image_surface_get_height sfc
	s <- c_cairo_image_surface_get_stride sfc
	p <- mallocBytes . fromIntegral $ s * h
	copyBytes p d . fromIntegral $ s * h
	fd <- newForeignPtr (castPtr p) $ free (castPtr p)
	pure $ CairoImageMut f (fromIntegral w) (fromIntegral h) (fromIntegral s) fd

newtype Argb32 = Argb32 Word32 deriving (Show, Storable)

foreign import ccall "cairo_image_surface_create_for_data" c_cairo_image_surface_create_for_data ::
	Ptr #{type unsigned char} -> #{type cairo_format_t} -> #{type int} -> #{type int} -> #{type int} -> IO (Ptr (CairoSurfaceT s))

cairoImageSurfaceCreateForCairoImage ::
	PrimMonad m => CairoImage -> m (CairoSurfaceImageT (PrimState m))
cairoImageSurfaceCreateForCairoImage (CairoImage f w h s d) = unsafeIOToPrim do
	p <- mallocBytes n
	withForeignPtr d \pd -> copyBytes p pd n
	sp <- c_cairo_image_surface_create_for_data (castPtr p) f (fromIntegral w) (fromIntegral h) (fromIntegral s)
	mkCairoSurfaceImageT' sp p
	where n = fromIntegral $ s * h

cairoImageSurfaceCreateForCairoImageMut ::
	PrimMonad m => CairoImageMut (PrimState m) -> m (CairoSurfaceImageT (PrimState m))
cairoImageSurfaceCreateForCairoImageMut (CairoImageMut f w h s d) = unsafeIOToPrim do
	p <- mallocBytes n
	withForeignPtr d \pd -> copyBytes p pd n
	sp <- c_cairo_image_surface_create_for_data (castPtr p) f (fromIntegral w) (fromIntegral h) (fromIntegral s)
	mkCairoSurfaceImageT' sp p
	where n = fromIntegral $ s * h

foreign import ccall "cairo_image_surface_get_data" c_cairo_image_surface_get_data ::
	Ptr (CairoSurfaceT s) -> IO (Ptr #{type unsigned char})

argCairoSurfaceT :: PrimMonad m => (Ptr (CairoSurfaceT (PrimState m)) -> IO a) -> CairoSurfaceImageT (PrimState m) -> m a
argCairoSurfaceT io (CairoSurfaceImageT fs) = unsafeIOToPrim $ withForeignPtr fs io

returnCairoSurfaceT :: PrimMonad m => IO (Ptr (CairoSurfaceT (PrimState m))) -> m (CairoSurfaceImageT (PrimState m))
returnCairoSurfaceT io = unsafeIOToPrim $ mkCairoSurfaceImageT =<< io

mkCairoSurfaceImageT :: Ptr (CairoSurfaceT s) -> IO (CairoSurfaceImageT s)
mkCairoSurfaceImageT p = CairoSurfaceImageT <$> newForeignPtr p (c_cairo_surface_destroy p)

mkCairoSurfaceImageT' :: Ptr (CairoSurfaceT s) -> Ptr a -> IO (CairoSurfaceImageT s)
mkCairoSurfaceImageT' ps p = CairoSurfaceImageT <$> newForeignPtr ps (free p >> c_cairo_surface_destroy ps)
