{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.CairoImage where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Control.Monad.Primitive
-- import Data.Vector
import Data.Word
import Data.Int
import System.IO.Unsafe

-- import Graphics.Cairo.Values
import Graphics.Cairo.Monad

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

{-
class Storable p => CairoPixel p where
	posToPtr :: #{type int} -> #{type int} -> Ptr p -> Ptr p
	-}

pattern CairoImageArgb32 :: Argb32 -> CairoImage
pattern CairoImageArgb32 a <- (cairoImageToArgb32 -> Just a)

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
	pixelAt :: i -> #{type int} -> #{type int} -> Maybe (Pixel i)

class ImageMut im where
	type PixelMut im
	getPixel :: PrimMonad m =>
		im (PrimState m) -> #{type int} -> #{type int} -> m (Maybe (PixelMut im))
	putPixel :: PrimMonad m =>
		im (PrimState m) -> #{type int} -> #{type int} -> PixelMut im -> m ()

instance Image Argb32 where
	type Pixel Argb32 = PixelArgb32
	pixelAt (Argb32 w h s d) x y = unsafePerformIO do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . peek) $ ptrArgb32 w h s p x y

instance ImageMut Argb32Mut where
	type PixelMut Argb32Mut = PixelArgb32
	getPixel (Argb32Mut w h s d) x y = unPrimIo do
		withForeignPtr d \p -> maybe (pure Nothing) ((Just <$>) . peek) $ ptrArgb32 w h s p x y
	putPixel (Argb32Mut w h s d) x y px = unPrimIo do
		withForeignPtr d \p -> maybe (pure ()) (`poke` px) $ ptrArgb32 w h s p x y

ptrArgb32 :: #{type int} -> #{type int} -> #{type int} ->
	Ptr PixelArgb32 -> #{type int} -> #{type int} -> Maybe (Ptr PixelArgb32)
ptrArgb32 w h s p x y
	| 0 <= x && x < w && 0 <= y && y < h = Just $ p `plusPtr` fromIntegral (y * s + x * 4)
	| otherwise = Nothing

-- newtype ImageArgb32 = ImageArgb32 (ForeignPtr Word32) deriving Show

-- loadImageArgb32 :: Ptr #{type unsigned char} -> IO ImageArgb32
-- loadImageArgb32 p = 
