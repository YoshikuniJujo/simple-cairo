{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.CairoImage.Internal (
	-- * Class Image and ImageMut
	Image(..), ImageMut(..),
	-- * Type CairoImage and CairoImageMut
	CairoImage(..), CairoImageMut(..), cairoImageFreeze, cairoImageThaw,
	-- * Image Format
	-- ** ARGB32
	PixelArgb32(..), pattern PixelArgb32,
	pattern CairoImageArgb32, Argb32,
	pattern CairoImageMutArgb32, Argb32Mut,
	-- ** RGB24
	PixelRgb24(..), pattern PixelRgb24,
	pattern CairoImageRgb24, Rgb24,
	pattern CairoImageMutRgb24, Rgb24Mut
	) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Foldable
import Data.Bits
import Data.Word
import Data.Int
import System.IO.Unsafe

#include <cairo.h>

foreign import ccall "cairo_format_stride_for_width"
	c_cairo_format_stride_for_width ::
	#{type cairo_format_t} -> #{type int} -> IO #{type int}

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

cairoImageDataCopy :: #{type int} -> #{type int} ->
	ForeignPtr #{type unsigned char} -> IO (ForeignPtr #{type unsigned char})
cairoImageDataCopy str h fdt = withForeignPtr fdt \dt -> do
	dt' <- mallocBytes . fromIntegral $ str * h
	copyBytes dt' dt . fromIntegral $ str * h
	newForeignPtr dt' (free dt')

cairoImageFreeze :: PrimMonad m =>
	CairoImageMut (PrimState m) -> m CairoImage
cairoImageFreeze cim = unsafeIOToPrim
	$ CairoImage fmt w h str <$> cairoImageDataCopy str h dt
	where
	fmt = cairoImageMutFormat cim
	w = cairoImageMutWidth cim
	h = cairoImageMutHeight cim
	str = cairoImageMutStride cim
	dt = cairoImageMutData cim

cairoImageThaw :: PrimMonad m =>
	CairoImage -> m (CairoImageMut (PrimState m))
cairoImageThaw ci = unsafeIOToPrim
	$ CairoImageMut fmt w h str <$> cairoImageDataCopy str h dt
	where
	fmt = cairoImageFormat ci
	w = cairoImageWidth ci
	h = cairoImageHeight ci
	str = cairoImageStride ci
	dt = cairoImageData ci

instance Eq CairoImage where
	ci1 == ci2 = and [
		fmt1 == fmt2, w1 == w2, h1 == h2, str1 == str2,
		unsafePerformIO
			$ withForeignPtr fd1 \d1 -> withForeignPtr fd2 \d2 ->
				compareBytes d1 d2 (str1 * h1) >>=
					\case EQ -> pure True; _ -> pure False ]
		where
		[fmt1, fmt2] = cairoImageFormat <$> [ci1, ci2]
		[w1, w2] = cairoImageWidth <$> [ci1, ci2]
		[h1, h2] = cairoImageHeight <$> [ci1, ci2]
		[str1, str2] = cairoImageStride <$> [ci1, ci2]
		[fd1, fd2] = cairoImageData <$> [ci1, ci2]

compareBytes :: (Ord n, Num n) => Ptr a -> Ptr a -> n -> IO Ordering
compareBytes _ _ n | n < 1 = pure EQ
compareBytes p1 p2 _ | p1 == p2 = pure EQ
compareBytes p1 p2 n = compare <$> peek p1b <*> peek p2b >>=
	\case EQ -> compareBytes p1 p2 (n - 1); o -> pure o
	where [p1b, p2b] = castPtr <$> [p1, p2] :: [Ptr Word8]

pattern CairoImageArgb32 :: Argb32 -> CairoImage
pattern CairoImageArgb32 a <- (cairoImageToArgb32 -> Just a)
	where CairoImageArgb32 (Argb32 w h s d) =
		CairoImage #{const CAIRO_FORMAT_ARGB32} w h s $ castForeignPtr d

cairoImageToArgb32 :: CairoImage -> Maybe Argb32
cairoImageToArgb32 = \case
	CairoImage #{const CAIRO_FORMAT_ARGB32} w h s d ->
		Just . Argb32 w h s $ castForeignPtr d
	_ -> Nothing

pattern CairoImageRgb24 :: Rgb24 -> CairoImage
pattern CairoImageRgb24 r <- (cairoImageToRgb24 -> Just r)
	where CairoImageRgb24 (Rgb24 w h s d) =
		CairoImage #{const CAIRO_FORMAT_RGB24} w h s $ castForeignPtr d

cairoImageToRgb24 :: CairoImage -> Maybe Rgb24
cairoImageToRgb24 = \case
	CairoImage #{const CAIRO_FORMAT_RGB24} w h s d ->
		Just . Rgb24 w h s $ castForeignPtr d
	_ -> Nothing

pattern CairoImageMutRgb24 :: Rgb24Mut s -> CairoImageMut s
pattern CairoImageMutRgb24 r <- (cairoImageMutToRgb24 -> Just r)
	where CairoImageMutRgb24 (Rgb24Mut w h s d) =
		CairoImageMut #{const CAIRO_FORMAT_RGB24} w h s $ castForeignPtr d

cairoImageMutToRgb24 :: (CairoImageMut s) -> Maybe (Rgb24Mut s)
cairoImageMutToRgb24 = \case
	CairoImageMut #{const CAIRO_FORMAT_RGB24} w h s d ->
		Just . Rgb24Mut w h s $ castForeignPtr d
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

newtype PixelArgb32 = PixelArgb32Word32 Word32 deriving (Show, Storable)

{-# COMPLETE PixelArgb32 #-}

pattern PixelArgb32 :: Word8 -> Word8 -> Word8 -> Word8 -> PixelArgb32
pattern PixelArgb32 a r g b <- (pixelArgb32ToArgb -> (a, r, g, b))
	where PixelArgb32 = pixelArgb32FromArgb

pixelArgb32FromArgb :: Word8 -> Word8 -> Word8 -> Word8 -> PixelArgb32
pixelArgb32FromArgb
	(fromIntegral -> a) (fromIntegral -> r)
	(fromIntegral -> g) (fromIntegral -> b) = PixelArgb32Word32
	$ a `shiftL` 24 .|. r `shiftL` 16 .|. g `shift` 8 .|. b

pixelArgb32ToArgb :: PixelArgb32 -> (Word8, Word8, Word8, Word8)
pixelArgb32ToArgb (PixelArgb32Word32 w) = (
	fromIntegral $ w `shiftR` 24, fromIntegral $ w `shiftR` 16,
	fromIntegral $ w `shiftR` 8, fromIntegral w )

class Image i where
	type Pixel i
	imageSize :: i -> (#{type int}, #{type int})
	pixelAt :: i -> #{type int} -> #{type int} -> Maybe (Pixel i)
	generateImage :: #{type int} -> #{type int} -> (#{type int} -> #{type int} -> Pixel i) -> i
	generateImagePrimM :: PrimBase m => #{type int} -> #{type int} -> (#{type int} -> #{type int} -> m (Pixel i)) -> m i

	generateImage w h f = runST $ generateImagePrimM w h (\x y -> pure $ f x y)

class ImageMut im where
	type PixelMut im
	imageMutSize :: im s -> (#{type int}, #{type int})
	getPixel :: PrimMonad m =>
		im (PrimState m) -> #{type int} -> #{type int} -> m (Maybe (PixelMut im))
	newImageMut :: PrimMonad m =>
		#{type int} -> #{type int} -> m (im (PrimState m))
	putPixel :: PrimMonad m =>
		im (PrimState m) -> #{type int} -> #{type int} -> PixelMut im -> m ()

instance Image Argb32 where
	type Pixel Argb32 = PixelArgb32
	imageSize (Argb32 w h _ _) = (w, h)
	generateImagePrimM = generateArgb32PrimM
	pixelAt (Argb32 w h s d) x y = unsafePerformIO do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . peek) $ ptrArgb32 w h s p x y

instance Image Rgb24 where
	type Pixel Rgb24 = PixelRgb24
	imageSize (Rgb24 w h _ _) = (w, h)
	generateImagePrimM = generateRgb24PrimM
	pixelAt (Rgb24 w h s d) x y = unsafePerformIO do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . peek) $ ptrRgb24 w h s p x y

generateArgb32PrimM :: PrimBase	m => #{type int} -> #{type int} -> (#{type int} -> #{type int} -> m PixelArgb32) -> m Argb32
generateArgb32PrimM w h f = unsafeIOToPrim do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_ARGB32} w
	d <- mallocBytes . fromIntegral $ s * h
	for_ [0 .. h - 1] \y -> for_ [0 .. w - 1] \x -> do
		p <- unsafePrimToIO $ f x y
		maybe (pure ()) (`poke` p) $ ptrArgb32 w h s d x y
	fd <- newForeignPtr d $ free d
	pure $ Argb32 w h s fd

generateRgb24PrimM :: PrimBase m => #{type int} -> #{type int} -> (#{type int} -> #{type int} -> m PixelRgb24) -> m Rgb24
generateRgb24PrimM w h f = unsafeIOToPrim do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_RGB24} w
	d <- mallocBytes . fromIntegral $ s * h
	for_ [0 .. h - 1] \y -> for_ [0 .. w - 1] \x -> do
		p <- unsafePrimToIO $ f x y
		maybe (pure ()) (`poke` p) $ ptrRgb24 w h s d x y
	fd <- newForeignPtr d $ free d
	pure $ Rgb24 w h s fd

instance ImageMut Argb32Mut where
	type PixelMut Argb32Mut = PixelArgb32
	imageMutSize (Argb32Mut w h _ _) = (w, h)
	newImageMut = newArgb32Mut
	getPixel (Argb32Mut w h s d) x y = unsafeIOToPrim do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . peek) $ ptrArgb32 w h s p x y
	putPixel (Argb32Mut w h s d) x y px = unsafeIOToPrim do
		withForeignPtr d \p -> maybe (pure ()) (`poke` px) $ ptrArgb32 w h s p x y

newArgb32Mut :: PrimMonad m => #{type int} -> #{type int} -> m (Argb32Mut (PrimState m))
newArgb32Mut w h = unsafeIOToPrim do
	s <- c_cairo_format_stride_for_width #{const CAIRO_FORMAT_ARGB32} w
	d <- mallocBytes . fromIntegral $ s * h
	fd <- newForeignPtr d $ free d
	pure $ Argb32Mut w h s fd

ptrArgb32 :: #{type int} -> #{type int} -> #{type int} ->
	Ptr PixelArgb32 -> #{type int} -> #{type int} -> Maybe (Ptr PixelArgb32)
ptrArgb32 w h s p x y
	| 0 <= x && x < w && 0 <= y && y < h = Just $ p `plusPtr` fromIntegral (y * s + x * 4)
	| otherwise = Nothing

ptrRgb24 :: #{type int} -> #{type int} -> #{type int} ->
	Ptr PixelRgb24 -> #{type int} -> #{type int} -> Maybe (Ptr PixelRgb24)
ptrRgb24 w h s p x y
	| x <= x && x < w && 0 <= y && y < h = Just $ p `plusPtr` fromIntegral (y * s + x * 4)
	| otherwise = Nothing

newtype PixelRgb24 = PixelRgb24Word32 Word32 deriving (Show, Storable)

pattern PixelRgb24 :: Word8 -> Word8 -> Word8 -> PixelRgb24
pattern PixelRgb24 r g b <- (pixelRgb24ToRgb -> (r, g, b))
	where PixelRgb24 = pixelRgb24FromRgb

pixelRgb24FromRgb :: Word8 -> Word8 -> Word8 -> PixelRgb24
pixelRgb24FromRgb (fromIntegral -> r)
	(fromIntegral -> g) (fromIntegral -> b) =
	PixelRgb24Word32 $ r `shiftL` 16 .|. g `shift` 8 .|. b

pixelRgb24ToRgb :: PixelRgb24 -> (Word8, Word8, Word8)
pixelRgb24ToRgb (PixelRgb24Word32 w) = (
	fromIntegral $ w `shiftR` 16,
	fromIntegral $ w `shiftR` 8, fromIntegral w )

data Rgb24 = Rgb24 {
	rgb24Width :: #{type int}, rgb24Height :: #{type int},
	rgb24Stride :: #{type int}, rgb24Data :: ForeignPtr PixelRgb24 }
	deriving Show

data Rgb24Mut s = Rgb24Mut {
	rgb24MutWidth :: #{type int}, rgb24MutHeight :: #{type int},
	rgb24MutStride :: #{type int}, rgb24MutData :: ForeignPtr PixelRgb24 }
	deriving Show
