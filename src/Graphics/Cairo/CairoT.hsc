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

import Graphics.Cairo.Exception
import Graphics.Cairo.Monad
import Graphics.Cairo.Types

foreign import ccall "cairo_create" c_cairo_create :: Ptr (CairoSurfaceT s) -> IO (Ptr (CairoT s))

cairoCreate :: CairoMonad s m => CairoSurfaceT s -> m (CairoT s)
cairoCreate = returnCairoT . argCairoSurfaceT c_cairo_create

foreign import ccall "cairo_set_line_width" c_cairo_set_line_width ::
	Ptr (CairoT s) -> #{type double} -> IO ()

cairoSetLineWidth :: CairoMonad s m => CairoT s -> #{type double} -> m ()
cairoSetLineWidth cr w = (`argCairoT` cr) \pcr -> c_cairo_set_line_width pcr w

foreign import ccall "cairo_set_source_rgb" c_cairo_set_source_rgb ::
	Ptr (CairoT s) -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoSetSourceRgb :: CairoMonad s m => CairoT s -> #{type double} -> #{type double} -> #{type double} -> m ()
cairoSetSourceRgb cr r g b = argCairoT (\pcr -> c_cairo_set_source_rgb pcr r g b) cr

foreign import ccall "cairo_set_source_rgba" c_cairo_set_source_rgba ::
	Ptr (CairoT s) -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoSetSourceRgba :: CairoMonad s m => CairoT s -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> m ()
cairoSetSourceRgba cr r g b a = argCairoT (\pcr -> c_cairo_set_source_rgba pcr r g b a) cr

foreign import ccall "cairo_stroke" c_cairo_stroke :: Ptr (CairoT s) -> IO ()

cairoStroke :: CairoMonad s m => CairoT s -> m ()
cairoStroke = argCairoT c_cairo_stroke

foreign import ccall "cairo_fill" c_cairo_fill :: Ptr (CairoT s) -> IO ()

cairoFill :: CairoMonad s m => CairoT s -> m ()
cairoFill = argCairoT c_cairo_fill

foreign import ccall "cairo_paint" c_cairo_paint ::
	Ptr (CairoT s) -> IO ()

cairoPaint :: CairoMonad s m => CairoT s -> m ()
cairoPaint = argCairoT c_cairo_paint

foreign import ccall "cairo_paint_with_alpha" c_cairo_paint_with_alpha ::
	Ptr (CairoT s) -> #{type double} -> IO ()

cairoPaintWithAlpha :: CairoMonad s m => CairoT s -> #{type double} -> m ()
cairoPaintWithAlpha cr a = argCairoT (\pcr -> c_cairo_paint_with_alpha pcr a) cr

foreign import ccall "cairo_set_source" c_cairo_set_source ::
	Ptr (CairoT s) -> Ptr (CairoPatternT s) -> IO ()

cairoSetSource :: CairoMonad s m => CairoT s -> CairoPatternT s -> m ()
cairoSetSource cr pt = (`argCairoT` cr) \pcr -> (`argCairoPatternT` pt) \ppt ->
		c_cairo_set_source pcr ppt

foreign import ccall "cairo_mask" c_cairo_mask ::
	Ptr (CairoT s) -> Ptr (CairoPatternT s) -> IO ()

cairoMask :: CairoMonad s m => CairoT s -> CairoPatternT s -> m ()
cairoMask cr pt = (`argCairoT` cr) \pcr -> (`argCairoPatternT` pt) \ppt ->
		c_cairo_mask pcr ppt

foreign import ccall "cairo_push_group" c_cairo_push_group ::
	Ptr (CairoT s) -> IO ()

cairoPushGroup :: CairoMonad s m => CairoT s -> m ()
cairoPushGroup = argCairoT c_cairo_push_group

foreign import ccall "cairo_pop_group_to_source" c_cairo_pop_group_to_source ::
	Ptr (CairoT s) -> IO ()

cairoPopGroupToSource :: CairoMonad s m => CairoT s -> m ()
cairoPopGroupToSource cr = argCairoT c_cairo_pop_group_to_source cr <* raiseIfError cr

foreign import ccall "cairo_pop_group" c_cairo_pop_group ::
	Ptr (CairoT s) -> IO (Ptr (CairoPatternT s))

cairoPopGroup :: CairoMonad s m => CairoT s -> m (CairoPatternT s)
cairoPopGroup = returnCairoPatternT . argCairoT c_cairo_pop_group
