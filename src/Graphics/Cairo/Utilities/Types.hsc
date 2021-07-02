{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Utilities.Types where

import Foreign.Storable
import Data.Int

#include <cairo.h>

data CairoRectangleIntT = CairoRectangleIntT {
	cairoRectangleIntTX, cairoRectangleIntTY :: #{type int},
	cairoREctangleIntTWidth, cairoRectangleIntTHeight :: #{type int} }
	deriving Show

instance Storable CairoRectangleIntT where
	sizeOf _ = #size cairo_rectangle_int_t
	alignment _ = #alignment cairo_rectangle_int_t
	peek p = CairoRectangleIntT
		<$> #{peek cairo_rectangle_int_t, x} p
		<*> #{peek cairo_rectangle_int_t, y} p
		<*> #{peek cairo_rectangle_int_t, width} p
		<*> #{peek cairo_rectangle_int_t, height} p
	poke p (CairoRectangleIntT x y w h) = do
		#{poke cairo_rectangle_int_t, x} p x
		#{poke cairo_rectangle_int_t, y} p y
		#{poke cairo_rectangle_int_t, width} p w
		#{poke cairo_rectangle_int_t, height} p h
