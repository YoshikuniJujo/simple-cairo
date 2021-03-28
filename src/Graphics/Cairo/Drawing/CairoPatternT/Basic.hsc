{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoPatternT.Basic where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C.Types
import Control.Monad.Primitive
import Data.Word
import Data.Color

#include <cairo.h>

newtype CairoPatternT s = CairoPatternT (ForeignPtr (CairoPatternT s)) deriving Show

makeCairoPatternT :: Ptr (CairoPatternT s) -> IO (CairoPatternT s)
makeCairoPatternT p = CairoPatternT <$> newForeignPtr p (c_cairo_pattern_destroy p)

returnCairoPatternT :: PrimMonad m => IO (Ptr (CairoPatternT (PrimState m))) -> m (CairoPatternT (PrimState m))
returnCairoPatternT io = unsafeIOToPrim $ makeCairoPatternT =<< io

foreign import ccall "cairo_pattern_destroy" c_cairo_pattern_destroy ::
	Ptr (CairoPatternT s) -> IO ()

newtype CairoPatternTypeT = CairoPatternTypeT #{type cairo_pattern_type_t} deriving Show

pattern CairoPatternTypeSolid :: CairoPatternTypeT
pattern CairoPatternTypeSolid <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_SOLID} where
	CairoPatternTypeSolid = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_SOLID}

pattern CairoPatternTypeSurface :: CairoPatternTypeT
pattern CairoPatternTypeSurface <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_SURFACE} where
	CairoPatternTypeSurface = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_SURFACE}

pattern CairoPatternTypeLinear :: CairoPatternTypeT
pattern CairoPatternTypeLinear <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_LINEAR} where
	CairoPatternTypeLinear = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_LINEAR}

pattern CairoPatternTypeRadial :: CairoPatternTypeT
pattern CairoPatternTypeRadial <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_RADIAL} where
	CairoPatternTypeRadial = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_RADIAL}

pattern CairoPatternTypeMesh :: CairoPatternTypeT
pattern CairoPatternTypeMesh <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_MESH} where
	CairoPatternTypeMesh = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_MESH}

pattern CairoPatternTypeRasterSource :: CairoPatternTypeT
pattern CairoPatternTypeRasterSource <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_RASTER_SOURCE} where
	CairoPatternTypeRasterSource = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_RASTER_SOURCE}

cairoPatternCreateRgb :: PrimMonad m => Rgb -> m (CairoPatternT (PrimState m))
cairoPatternCreateRgb (RgbDouble r g b) = returnCairoPatternT
	$ c_cairo_pattern_create_rgb r g b

foreign import ccall "cairo_pattern_create_rgb" c_cairo_pattern_create_rgb ::
	CDouble -> CDouble -> CDouble -> IO (Ptr (CairoPatternT s))
