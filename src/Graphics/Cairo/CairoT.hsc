{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.CairoT (
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
import Foreign.ForeignPtr hiding (newForeignPtr)

import Graphics.Cairo.Types

foreign import ccall "cairo_create" c_cairo_create :: Ptr (CairoSurfaceT s) -> IO (Ptr (CairoT s))

cairoCreate :: CairoSurfaceT s -> IO (CairoT s)
cairoCreate (CairoSurfaceT s) = makeCairoT =<< withForeignPtr s \p -> c_cairo_create p

foreign import ccall "cairo_set_line_width" c_cairo_set_line_width ::
	Ptr (CairoT s) -> #{type double} -> IO ()

cairoSetLineWidth :: CairoT s -> #{type double} -> IO ()
cairoSetLineWidth (CairoT cr) w = withForeignPtr cr \p -> c_cairo_set_line_width p w

foreign import ccall "cairo_set_source_rgb" c_cairo_set_source_rgb ::
	Ptr (CairoT s) -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoSetSourceRgb :: CairoT s -> #{type double} -> #{type double} -> #{type double} -> IO ()
cairoSetSourceRgb (CairoT cr) r g b = withForeignPtr cr \p -> c_cairo_set_source_rgb p r g b

foreign import ccall "cairo_set_source_rgba" c_cairo_set_source_rgba ::
	Ptr (CairoT s) -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoSetSourceRgba :: CairoT s -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()
cairoSetSourceRgba (CairoT cr) r g b a = withForeignPtr cr \p -> c_cairo_set_source_rgba p r g b a

foreign import ccall "cairo_stroke" c_cairo_stroke :: Ptr (CairoT s) -> IO ()

cairoStroke :: CairoT s -> IO ()
cairoStroke (CairoT cr) = withForeignPtr cr c_cairo_stroke

foreign import ccall "cairo_fill" c_cairo_fill :: Ptr (CairoT s) -> IO ()

cairoFill :: CairoT s -> IO ()
cairoFill (CairoT cr) = withForeignPtr cr c_cairo_fill

foreign import ccall "cairo_paint" c_cairo_paint ::
	Ptr (CairoT s) -> IO ()

cairoPaint :: CairoT s -> IO ()
cairoPaint (CairoT fcr) = withForeignPtr fcr c_cairo_paint

foreign import ccall "cairo_paint_with_alpha" c_cairo_paint_with_alpha ::
	Ptr (CairoT s) -> #{type double} -> IO ()

cairoPaintWithAlpha :: CairoT s -> #{type double} -> IO ()
cairoPaintWithAlpha (CairoT fcr) a = withForeignPtr fcr \cr -> c_cairo_paint_with_alpha cr a

foreign import ccall "cairo_set_source" c_cairo_set_source ::
	Ptr (CairoT s) -> Ptr (CairoPatternT s) -> IO ()

cairoSetSource :: CairoT s -> CairoPatternT s -> IO ()
cairoSetSource (CairoT fcr) (CairoPatternT fpt) =
	withForeignPtr fcr \cr -> withForeignPtr fpt \pt ->
		c_cairo_set_source cr pt

foreign import ccall "cairo_mask" c_cairo_mask ::
	Ptr (CairoT s) -> Ptr (CairoPatternT s) -> IO ()

cairoMask :: CairoT s -> CairoPatternT s -> IO ()
cairoMask (CairoT fcr) (CairoPatternT fpt) =
	withForeignPtr fcr \cr -> withForeignPtr fpt \pt ->
		c_cairo_mask cr pt

foreign import ccall "cairo_push_group" c_cairo_push_group ::
	Ptr (CairoT s) -> IO ()

cairoPushGroup :: CairoT s -> IO ()
cairoPushGroup (CairoT fcr) = withForeignPtr fcr c_cairo_push_group

foreign import ccall "cairo_pop_group_to_source" c_cairo_pop_group_to_source ::
	Ptr (CairoT s) -> IO ()

cairoPopGroupToSource :: CairoT s -> IO ()
cairoPopGroupToSource (CairoT fcr) = withForeignPtr fcr c_cairo_pop_group_to_source

foreign import ccall "cairo_pop_group" c_cairo_pop_group ::
	Ptr (CairoT s) -> IO (Ptr (CairoPatternT s))

cairoPopGroup :: CairoT s -> IO (CairoPatternT s)
cairoPopGroup (CairoT fcr) = makeCairoPatternT =<< withForeignPtr fcr c_cairo_pop_group
