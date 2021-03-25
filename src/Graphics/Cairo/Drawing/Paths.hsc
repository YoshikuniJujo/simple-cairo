{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Paths (
	cairoClosePath, cairoArc, cairoLineTo, cairoMoveTo, cairoRectangle,
	cairoRelCurveTo, cairoRelLineTo,

	cairoNewPath
	) where

import Foreign.Ptr
import Foreign.C.Types
import Control.Monad.Primitive

import Graphics.Cairo.Drawing.Paths.Basic

import Data.CairoContext

#include <cairo.h>

cairoRelLineTo :: PrimMonad m => CairoT (PrimState m) -> #{type double} -> #{type double} -> m ()
cairoRelLineTo cr x y = withCairoT cr \pcr -> c_cairo_rel_line_to pcr x y

foreign import ccall "cairo_rel_line_to" c_cairo_rel_line_to ::
	Ptr (CairoT s) -> #{type double} -> #{type double} -> IO ()

cairoArc :: PrimMonad m => CairoT (PrimState m) -> #{type double} -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> m ()
cairoArc cr xc yc r a1 a2 = 
	withCairoT cr \pcr -> c_cairo_arc pcr xc yc r a1 a2

foreign import ccall "cairo_arc" c_cairo_arc ::
	Ptr (CairoT s) -> #{type double} -> #{type double} ->
	#{type double} -> #{type double} -> #{type double} -> IO ()

cairoRelCurveTo :: PrimMonad m => CairoT (PrimState m) -> #{type double} -> #{type double} ->
	#{type double} -> #{type double} ->
	#{type double} -> #{type double} -> m ()
cairoRelCurveTo cr dx1 dy1 dx2 dy2 dx3 dy3 = withCairoT cr \pcr ->
	c_cairo_rel_curve_to pcr dx1 dy1 dx2 dy2 dx3 dy3

foreign import ccall "cairo_rel_curve_to" c_cairo_rel_curve_to ::
	Ptr (CairoT s) -> #{type double} -> #{type double} ->
		#{type double} -> #{type double} ->
		#{type double} -> #{type double} -> IO ()

cairoClosePath :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoClosePath = (`withCairoT` c_cairo_close_path)

foreign import ccall "cairo_close_path" c_cairo_close_path ::
	Ptr (CairoT s) -> IO ()

foreign import ccall "cairo_rectangle" c_cairo_rectangle ::
	Ptr (CairoT s) -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoRectangle :: PrimMonad m => CairoT (PrimState m) -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> m ()
cairoRectangle cr x y w h = withCairoT cr \pcr -> c_cairo_rectangle pcr x y w h
