{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoPatternT (
	cairoPatternAddColorStopRgb, cairoPatternAddColorStopRgba,
	cairoPatternCreateLinear, cairoPatternCreateRadial,
	cairoPatternCreateForSurface
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Primitive

import Graphics.Cairo.Surfaces.CairoSurfaceT

import Graphics.Cairo.Drawing.CairoPatternT.Basic

#include <cairo.h>

foreign import ccall "cairo_pattern_create_radial" c_cairo_pattern_create_radial ::
	#{type double} -> #{type double} -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> IO (Ptr (CairoPatternT s))

cairoPatternCreateRadial :: PrimMonad m =>
	#{type double} -> #{type double} -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> m (CairoPatternT (PrimState m))
cairoPatternCreateRadial cx0 cy0 r0 cx1 cy1 r1 = returnCairoPatternT
	$ c_cairo_pattern_create_radial cx0 cy0 r0 cx1 cy1 r1

foreign import ccall "cairo_pattern_create_for_surface" c_cairo_pattern_create_for_surface ::
	Ptr (CairoSurfaceT s) -> IO (Ptr (CairoPatternT s))

cairoPatternCreateForSurface :: PrimMonad m =>
	CairoSurfaceT (PrimState m) -> m (CairoPatternT (PrimState m))
cairoPatternCreateForSurface (CairoSurfaceT fs) = unsafeIOToPrim
	$ withForeignPtr fs \s -> makeCairoPatternT =<< c_cairo_pattern_create_for_surface s
