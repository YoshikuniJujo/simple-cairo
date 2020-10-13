{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Paths (
	cairoClosePath, cairoArc, cairoLineTo, cairoMoveTo, cairoRectangle,
	cairoRelCurveTo, cairoRelLineTo
	) where

import Foreign.Ptr

import Graphics.Cairo.Monad
import Graphics.Cairo.Types

#include <cairo.h>

cairoMoveTo, cairoLineTo, cairoRelLineTo :: CairoMonad s m => CairoT s -> #{type double} -> #{type double} -> m ()
cairoMoveTo cr x y = (`argCairoT` cr) \pcr -> c_cairo_move_to pcr x y
cairoLineTo cr x y = (`argCairoT` cr) \pcr -> c_cairo_line_to pcr x y
cairoRelLineTo cr x y = (`argCairoT` cr) \pcr -> c_cairo_rel_line_to pcr x y

foreign import ccall "cairo_move_to" c_cairo_move_to ::
	Ptr (CairoT s) -> #{type double} -> #{type double} -> IO ()

foreign import ccall "cairo_line_to" c_cairo_line_to ::
	Ptr (CairoT s) -> #{type double} -> #{type double} -> IO ()

foreign import ccall "cairo_rel_line_to" c_cairo_rel_line_to ::
	Ptr (CairoT s) -> #{type double} -> #{type double} -> IO ()

cairoArc :: CairoMonad s m => CairoT s -> #{type double} -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> m ()
cairoArc cr xc yc r a1 a2 = 
	(`argCairoT` cr) \pcr -> c_cairo_arc pcr xc yc r a1 a2

foreign import ccall "cairo_arc" c_cairo_arc ::
	Ptr (CairoT s) -> #{type double} -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> IO ()

cairoRelCurveTo :: CairoMonad s m => CairoT s -> #{type double} -> #{type double} ->
	#{type double} -> #{type double} ->
	#{type double} -> #{type double} -> m ()
cairoRelCurveTo cr dx1 dy1 dx2 dy2 dx3 dy3 = (`argCairoT` cr) \pcr ->
	c_cairo_rel_curve_to pcr dx1 dy1 dx2 dy2 dx3 dy3

foreign import ccall "cairo_rel_curve_to" c_cairo_rel_curve_to ::
	Ptr (CairoT s) -> #{type double} -> #{type double} ->
		#{type double} -> #{type double} ->
		#{type double} -> #{type double} -> IO ()

cairoClosePath :: CairoMonad s m => CairoT s -> m ()
cairoClosePath = argCairoT c_cairo_close_path

foreign import ccall "cairo_close_path" c_cairo_close_path ::
	Ptr (CairoT s) -> IO ()

foreign import ccall "cairo_rectangle" c_cairo_rectangle ::
	Ptr (CairoT s) -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoRectangle :: CairoMonad s m => CairoT s -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> m ()
cairoRectangle cr x y w h = (`argCairoT` cr) \pcr -> c_cairo_rectangle pcr x y w h
