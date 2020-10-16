{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.CairoImage where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
-- import Data.Vector
import Data.Word
import Data.Int

-- import Graphics.Cairo.Values

#include <cairo.h>

data CairoImage = CairoImage {
	cairoImageFormat :: #{type cairo_format_t},
	cairoImageWidth :: #{type int},
	cairoImageHeight :: #{type int},
	cairoImageStride :: #{type int},
	cairoImageData :: ForeignPtr #{type unsigned char} }
	deriving Show

class Storable p => CairoPixel p where
	posToPtr :: #{type int} -> #{type int} -> Ptr p -> Ptr p

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

newtype PixelArgb32 = PixelArgb32 Word32 deriving Show

-- newtype ImageArgb32 = ImageArgb32 (ForeignPtr Word32) deriving Show

-- loadImageArgb32 :: Ptr #{type unsigned char} -> IO ImageArgb32
-- loadImageArgb32 p = 
