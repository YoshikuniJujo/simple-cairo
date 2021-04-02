{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoPatternT (
	cairoPatternAddColorStopRgb, cairoPatternAddColorStopRgba,
	cairoPatternCreateLinear, cairoPatternCreateRadial,
	cairoPatternCreateForSurface
	) where

import Foreign.ForeignPtr
import Control.Monad.Primitive

import Graphics.Cairo.Surfaces.CairoSurfaceT

import Graphics.Cairo.Drawing.CairoPatternT.Basic

#include <cairo.h>

cairoPatternCreateForSurface :: PrimMonad m =>
	CairoSurfaceT (PrimState m) -> m (CairoPatternT (PrimState m))
cairoPatternCreateForSurface (CairoSurfaceT fs) = unsafeIOToPrim
	$ withForeignPtr fs \s -> makeCairoPatternT =<< c_cairo_pattern_create_for_surface s
