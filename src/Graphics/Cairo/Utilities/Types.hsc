{-# LANGUAGE TemplateHaskell, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Utilities.Types where

import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Struct

#include <cairo.h>

struct "CairoRectangleIntT" #{size cairo_rectangle_int_t}
	[	("x", ''CInt, [| #{peek cairo_rectangle_int_t, x} |],
			[| #{poke cairo_rectangle_int_t, x} |]),
		("y", ''CInt, [| #{peek cairo_rectangle_int_t, y} |],
			[| #{poke cairo_rectangle_int_t, y} |]),
		("width", ''CInt, [| #{peek cairo_rectangle_int_t, width} |],
			[| #{poke cairo_rectangle_int_t, width} |]),
		("height", ''CInt, [| #{peek cairo_rectangle_int_t, height} |],
			[| #{poke cairo_rectangle_int_t, height} |]) ]
	[''Show]
