{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.CairoImage.Internal (
	-- * Class Image and ImageMut
	Image(..), ImageMut(..),
	-- * Type CairoImage and CairoImageMut
	CairoImage(..), CairoImageMut(..),
	-- * Image Format
	-- ** ARGB32
	PixelArgb32(..),
	pattern CairoImageArgb32, Argb32,
	pattern CairoImageMutArgb32, Argb32Mut ) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Foldable
import Data.Word
import Data.Int
import System.IO.Unsafe

import Data.CairoImage.Private

#include <cairo.h>

data CairoImage = CairoImage {
	cairoImageFormat :: #{type cairo_format_t},
	cairoImageWidth :: #{type int},
	cairoImageHeight :: #{type int},
	cairoImageStride :: #{type int},
	cairoImageData :: ForeignPtr #{type unsigned char} }
	deriving Show

data CairoImageMut s = CairoImageMut {
	cairoImageMutFormat :: #{type cairo_format_t},
	cairoImageMutWidth :: #{type int},
	cairoImageMutHeight :: #{type int},
	cairoImageMutStride :: #{type int},
	cairoImageMutData :: ForeignPtr #{type unsigned char} }
	deriving Show

pattern CairoImageArgb32 :: Argb32 -> CairoImage
pattern CairoImageArgb32 a <- (cairoImageToArgb32 -> Just a)
	where CairoImageArgb32 (Argb32 w h s d) =
		CairoImage #{const CAIRO_FORMAT_ARGB32} w h s $ castForeignPtr d

cairoImageToArgb32 :: CairoImage -> Maybe Argb32
cairoImageToArgb32 = \case
	CairoImage #{const CAIRO_FORMAT_ARGB32} w h s d ->
		Just . Argb32 w h s $ castForeignPtr d
	_ -> Nothing

data Argb32 = Argb32 {
	argb32Width :: #{type int},
	argb32Height :: #{type int},
	argb32Stride :: #{type int},
	argb32Data :: ForeignPtr PixelArgb32 }
	deriving Show

pattern CairoImageMutArgb32 :: Argb32Mut s -> CairoImageMut s
pattern CairoImageMutArgb32 a <- (cairoImageMutToArgb32 -> Just a)
	where CairoImageMutArgb32 (Argb32Mut w h s d) =
		CairoImageMut #{const CAIRO_FORMAT_ARGB32} w h s $ castForeignPtr d

cairoImageMutToArgb32 :: CairoImageMut s -> Maybe (Argb32Mut s)
cairoImageMutToArgb32 = \case
	CairoImageMut #{const CAIRO_FORMAT_ARGB32} w h s d ->
		Just . Argb32Mut w h s $ castForeignPtr d
	_ -> Nothing

data Argb32Mut s = Argb32Mut {
	argb32MutWidth :: #{type int},
	argb32MutHeight :: #{type int},
	argb32MutStride :: #{type int},
	argb32MutData :: ForeignPtr PixelArgb32 }
	deriving Show

newtype PixelArgb32 = PixelArgb32 Word32 deriving (Show, Storable)

class Image i where
	type Pixel i
	imageSize :: i -> (#{type int}, #{type int})
	generateImagePrimM :: PrimBase m => #{type int} -> #{type int} -> (#{type int} -> #{type int} -> m (Pixel i)) -> m i
	pixelAt :: i -> #{type int} -> #{type int} -> Maybe (Pixel i)

	generateImage :: #{type int} -> #{type int} -> (#{type int} -> #{type int} -> Pixel i) -> i
	generateImage w h f = runST $ generateImagePrimM w h (\x y -> pure $ f x y)

class ImageMut im where
	type PixelMut im
	imageMutSize :: im s -> (#{type int}, #{type int})
	newImageMut :: PrimMonad m =>
		#{type int} -> #{type int} -> m (im (PrimState m))
	getPixel :: PrimMonad m =>
		im (PrimState m) -> #{type int} -> #{type int} -> m (Maybe (PixelMut im))
	putPixel :: PrimMonad m =>
		im (PrimState m) -> #{type int} -> #{type int} -> PixelMut im -> m ()

instance Image Argb32 where
	type Pixel Argb32 = PixelArgb32
	imageSize (Argb32 w h _ _) = (w, h)
	generateImagePrimM = generateArgb32PrimM
	pixelAt (Argb32 w h s d) x y = unsafePerformIO do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . peek) $ ptrArgb32 w h s p x y

foreign import ccall "cairo_format_stride_for_width" c_cairo_format_stride_for_width ::
	#{type cairo_format_t} -> #{type int} -> IO #{type int}

generateArgb32PrimM :: PrimBase	m => #{type int} -> #{type int} -> (#{type int} -> #{type int} -> m PixelArgb32) -> m Argb32
generateArgb32PrimM w h f = unPrimIo do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_ARGB32} w
	d <- mallocBytes . fromIntegral $ s * h
	for_ [0 .. h - 1] \y -> for_ [0 .. w - 1] \x -> do
		p <- primIo $ f x y
		maybe (pure ()) (`poke` p) $ ptrArgb32 w h s d x y
	fd <- newForeignPtr d $ free d
	pure $ Argb32 w h s fd

instance ImageMut Argb32Mut where
	type PixelMut Argb32Mut = PixelArgb32
	imageMutSize (Argb32Mut w h _ _) = (w, h)
	newImageMut = newArgb32Mut
	getPixel (Argb32Mut w h s d) x y = unPrimIo do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . peek) $ ptrArgb32 w h s p x y
	putPixel (Argb32Mut w h s d) x y px = unPrimIo do
		withForeignPtr d \p -> maybe (pure ()) (`poke` px) $ ptrArgb32 w h s p x y

newArgb32Mut :: PrimMonad m => #{type int} -> #{type int} -> m (Argb32Mut (PrimState m))
newArgb32Mut w h = unPrimIo do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_ARGB32} w
	d <- mallocBytes . fromIntegral $ s * h
	fd <- newForeignPtr d $ free d
	pure $ Argb32Mut w h s fd

ptrArgb32 :: #{type int} -> #{type int} -> #{type int} ->
	Ptr PixelArgb32 -> #{type int} -> #{type int} -> Maybe (Ptr PixelArgb32)
ptrArgb32 w h s p x y
	| 0 <= x && x < w && 0 <= y && y < h = Just $ p `plusPtr` fromIntegral (y * s + x * 4)
	| otherwise = Nothing
