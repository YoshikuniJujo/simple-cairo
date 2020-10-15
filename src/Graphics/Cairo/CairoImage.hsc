{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.CairoImage where

-- import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Vector
import Data.Word
import Data.Int

import Graphics.Cairo.Values

#include <cairo.h>

data CairoImage = CairoImage {
	cairoImageFormat :: #{type cairo_format_t},
	cairoImageWidth :: #{type int},
	cairoImageHeight :: #{type int},
	cairoImageStride :: #{type int},
	cairoImageData :: ForeignPtr #{type unsigned char} }
	deriving Show

newtype ImageArgb32 = ImageArgb32 (ForeignPtr Word32) deriving Show

-- loadImageArgb32 :: Ptr #{type unsigned char} -> IO ImageArgb32
-- loadImageArgb32 p = 
