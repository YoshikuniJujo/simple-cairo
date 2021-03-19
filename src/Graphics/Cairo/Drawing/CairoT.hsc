{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT (
	-- * Basic
	cairoCreate,
	-- * Push and Pop Group
	cairoPushGroup, cairoPopGroup, cairoPopGroupToSource,
	-- * Set Source
	cairoSetSourceRgb, cairoSetSourceRgba, cairoSetSource,
	-- * Set Attribute
	cairoSetLineWidth,
	-- * Verb
	cairoFill, cairoMask, cairoPaint, cairoPaintWithAlpha, cairoStroke,
	) where

#include <cairo.h>

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Primitive

import Graphics.Cairo.Exception
import Graphics.Cairo.Types

import Graphics.Cairo.Drawing.CairoT.Basic

import Data.CairoContext

foreign import ccall "cairo_set_line_width" c_cairo_set_line_width ::
	Ptr (CairoT s) -> #{type double} -> IO ()

cairoSetLineWidth :: PrimMonad m => CairoT (PrimState m) -> #{type double} -> m ()
cairoSetLineWidth cr w = withCairoT cr \pcr -> c_cairo_set_line_width pcr w

foreign import ccall "cairo_stroke" c_cairo_stroke :: Ptr (CairoT s) -> IO ()

cairoStroke :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoStroke = (`withCairoT` c_cairo_stroke)

foreign import ccall "cairo_fill" c_cairo_fill :: Ptr (CairoT s) -> IO ()

cairoFill :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoFill = (`withCairoT` c_cairo_fill)

foreign import ccall "cairo_paint" c_cairo_paint ::
	Ptr (CairoT s) -> IO ()

cairoPaint :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoPaint = (`withCairoT` c_cairo_paint)

foreign import ccall "cairo_paint_with_alpha" c_cairo_paint_with_alpha ::
	Ptr (CairoT s) -> #{type double} -> IO ()

cairoPaintWithAlpha :: PrimMonad m => CairoT (PrimState m) -> #{type double} -> m ()
cairoPaintWithAlpha cr a = withCairoT cr \pcr -> c_cairo_paint_with_alpha pcr a

foreign import ccall "cairo_mask" c_cairo_mask ::
	Ptr (CairoT s) -> Ptr (CairoPatternT s) -> IO ()

cairoMask :: PrimMonad m => CairoT s -> CairoPatternT s -> m ()
cairoMask (CairoT fcr) (CairoPatternT fpt) = unsafeIOToPrim
	$ withForeignPtr fcr \cr -> withForeignPtr fpt \pt -> c_cairo_mask cr pt

foreign import ccall "cairo_push_group" c_cairo_push_group ::
	Ptr (CairoT s) -> IO ()

cairoPushGroup :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoPushGroup = (`withCairoT` c_cairo_push_group)

foreign import ccall "cairo_pop_group_to_source" c_cairo_pop_group_to_source ::
	Ptr (CairoT s) -> IO ()

cairoPopGroupToSource :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoPopGroupToSource cr@(CairoT fcr) = unsafeIOToPrim $ withForeignPtr fcr c_cairo_pop_group_to_source <* raiseIfError cr

foreign import ccall "cairo_pop_group" c_cairo_pop_group ::
	Ptr (CairoT s) -> IO (Ptr (CairoPatternT s))

cairoPopGroup :: PrimMonad m => CairoT s -> m (CairoPatternT s)
cairoPopGroup (CairoT fcr) = unsafeIOToPrim $ makeCairoPatternT =<< withForeignPtr fcr c_cairo_pop_group
