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

foreign import ccall "cairo_pattern_add_color_stop_rgb" c_cairo_pattern_add_color_stop_rgb ::
	Ptr (CairoPatternT s) -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoPatternAddColorStopRgb :: PrimMonad m =>
	CairoPatternT (PrimState m) -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> m ()
cairoPatternAddColorStopRgb pt os r g b = argCairoPatternT
	(\ppt -> c_cairo_pattern_add_color_stop_rgb ppt os r g b) pt

foreign import ccall "cairo_pattern_add_color_stop_rgba" c_cairo_pattern_add_color_stop_rgba ::
	Ptr (CairoPatternT s) -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoPatternAddColorStopRgba :: PrimMonad m =>
	CairoPatternT (PrimState m) -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> #{type double} -> m ()
cairoPatternAddColorStopRgba pt os r g b a = argCairoPatternT
	(\ppt -> c_cairo_pattern_add_color_stop_rgba ppt os r g b a) pt

foreign import ccall "cairo_pattern_create_linear" c_cairo_pattern_create_linear ::
	#{type double} -> #{type double} -> #{type double} -> #{type double} -> IO (Ptr (CairoPatternT s))

cairoPatternCreateLinear :: PrimMonad m =>
	#{type double} -> #{type double} -> #{type double} -> #{type double} -> m (CairoPatternT (PrimState m))
cairoPatternCreateLinear x0 y0 x1 y1 = returnCairoPatternT
	$ c_cairo_pattern_create_linear x0 y0 x1 y1

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

argCairoPatternT :: PrimMonad m => (Ptr (CairoPatternT (PrimState m)) -> IO a) -> CairoPatternT (PrimState m) -> m a
argCairoPatternT io (CairoPatternT fpt) = unsafeIOToPrim $ withForeignPtr fpt io
