{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.CairoPatternT (
	cairoPatternAddColorStopRgb, cairoPatternAddColorStopRgba,
	cairoPatternCreateLinear, cairoPatternCreateRadial
	) where

import Foreign.Ptr

import Graphics.Cairo.Monad
import Graphics.Cairo.Types

#include <cairo.h>

foreign import ccall "cairo_pattern_add_color_stop_rgb" c_cairo_pattern_add_color_stop_rgb ::
	Ptr (CairoPatternT s) -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoPatternAddColorStopRgb :: CairoMonad s m =>
	CairoPatternT s -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> m ()
cairoPatternAddColorStopRgb pt os r g b = argCairoPatternT
	(\ppt -> c_cairo_pattern_add_color_stop_rgb ppt os r g b) pt

foreign import ccall "cairo_pattern_add_color_stop_rgba" c_cairo_pattern_add_color_stop_rgba ::
	Ptr (CairoPatternT s) -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoPatternAddColorStopRgba :: CairoMonad s m =>
	CairoPatternT s -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> #{type double} -> m ()
cairoPatternAddColorStopRgba pt os r g b a = argCairoPatternT
	(\ppt -> c_cairo_pattern_add_color_stop_rgba ppt os r g b a) pt

foreign import ccall "cairo_pattern_create_linear" c_cairo_pattern_create_linear ::
	#{type double} -> #{type double} -> #{type double} -> #{type double} -> IO (Ptr (CairoPatternT s))

cairoPatternCreateLinear :: CairoMonad s m =>
	#{type double} -> #{type double} -> #{type double} -> #{type double} -> m (CairoPatternT s)
cairoPatternCreateLinear x0 y0 x1 y1 = returnCairoPatternT
	$ c_cairo_pattern_create_linear x0 y0 x1 y1

foreign import ccall "cairo_pattern_create_radial" c_cairo_pattern_create_radial ::
	#{type double} -> #{type double} -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> IO (Ptr (CairoPatternT s))

cairoPatternCreateRadial :: CairoMonad s m =>
	#{type double} -> #{type double} -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> m (CairoPatternT s)
cairoPatternCreateRadial cx0 cy0 r0 cx1 cy1 r1 = returnCairoPatternT
	$ c_cairo_pattern_create_radial cx0 cy0 r0 cx1 cy1 r1
