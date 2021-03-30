{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT.Basic (
	cairoCreate,
	cairoSetSourceRgb, cairoSetSourceRgba, cairoSetSource, cairoSetSourceSurface,
	cairoStroke, cairoStrokePreserve, cairoStrokeExtents, cairoInStroke,
	cairoFill, cairoFillPreserve, cairoFillExtents, cairoInFill,
	cairoPaint, cairoPaintWithAlpha,
	cairoMask, cairoMaskSurface,

	CairoExtents(..),
	pattern CairoExtentsLeftTopWidthHeight, cairoExtentsLeft, cairoExtentsTop, cairoExtentsWidth, cairoExtentsHeight
	) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Monad.Primitive
import Data.Int

import Graphics.Cairo.Surfaces.CairoSurfaceT
import Graphics.Cairo.Exception
import Graphics.Cairo.Drawing.Extents

import Data.Color
import Data.CairoContext

import Graphics.Cairo.Drawing.CairoPatternT.Basic

#include <cairo.h>

cairoCreate :: PrimMonad m =>
	CairoSurfaceT (PrimState m) -> m (CairoT (PrimState m))
cairoCreate (CairoSurfaceT sr) = unsafeIOToPrim do
	cr <- withForeignPtr sr c_cairo_create >>= \pcr ->
		CairoT <$> newForeignPtr pcr (c_cairo_destroy pcr)
	cr <$ raiseIfError cr

foreign import ccall "cairo_create"
	c_cairo_create :: Ptr (CairoSurfaceT s) -> IO (Ptr (CairoT s))
foreign import ccall "cairo_destroy" c_cairo_destroy :: Ptr (CairoT s) -> IO ()

cairoSetSourceRgb :: PrimMonad m => CairoT (PrimState m) -> Rgb -> m ()
cairoSetSourceRgb cr (RgbDouble r g b) =
	withCairoT cr \pcr -> c_cairo_set_source_rgb pcr r g b

foreign import ccall "cairo_set_source_rgb" c_cairo_set_source_rgb ::
	Ptr (CairoT s) -> CDouble -> CDouble -> CDouble -> IO ()

cairoSetSourceRgba :: PrimMonad m => CairoT (PrimState m) -> Rgba -> m ()
cairoSetSourceRgba cr (RgbaDouble r g b a) =
	withCairoT cr \pcr -> c_cairo_set_source_rgba pcr r g b a

foreign import ccall "cairo_set_source_rgba" c_cairo_set_source_rgba ::
	Ptr (CairoT s) -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

cairoSetSource :: PrimMonad m => CairoT s -> CairoPatternT s -> m ()
cairoSetSource (CairoT fcr) pt@(CairoPatternT fpt) = unsafeIOToPrim
	$ withForeignPtr fcr \pcr -> withForeignPtr fpt \ppt -> c_cairo_set_source pcr ppt >> raiseIfErrorPattern pt

foreign import ccall "cairo_set_source" c_cairo_set_source ::
	Ptr (CairoT s) -> Ptr (CairoPatternT s) -> IO ()

cairoSetSourceSurface :: PrimMonad m => CairoT s -> CairoSurfaceT s -> CDouble -> CDouble -> m ()
cairoSetSourceSurface (CairoT fcr) (CairoSurfaceT fsr) x y = unsafeIOToPrim
	$ withForeignPtr fcr \cr -> withForeignPtr fsr \sr -> c_cairo_set_source_surface cr sr x y

foreign import ccall "cairo_set_source_surface" c_cairo_set_source_surface ::
	Ptr (CairoT s) -> Ptr (CairoSurfaceT s) -> CDouble -> CDouble -> IO ()

cairoStroke :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoStroke = (`withCairoT` c_cairo_stroke)

foreign import ccall "cairo_stroke" c_cairo_stroke :: Ptr (CairoT s) -> IO ()

cairoStrokePreserve :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoStrokePreserve = (`withCairoT` c_cairo_stroke_preserve)

foreign import ccall "cairo_stroke_preserve" c_cairo_stroke_preserve :: Ptr (CairoT s) -> IO ()

cairoStrokeExtents :: PrimMonad m => CairoT (PrimState m) -> m CairoExtents
cairoStrokeExtents = flip withCairoT \pcr -> alloca \x1 -> alloca \y1 -> alloca \x2 -> alloca \y2 -> do
	c_cairo_stroke_extents pcr x1 y1 x2 y2
	CairoExtentsLeftTopRightBottom <$> peek x1 <*> peek y1 <*> peek x2 <*> peek y2

foreign import ccall "cairo_stroke_extents" c_cairo_stroke_extents ::
	Ptr (CairoT s) -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()

cairoInStroke :: PrimMonad m => CairoT (PrimState m) -> CDouble -> CDouble -> m Bool
cairoInStroke cr x y = withCairoT cr \pcr -> (/= 0) <$> c_cairo_in_stroke pcr x y

foreign import ccall "cairo_in_stroke" c_cairo_in_stroke ::
	Ptr (CairoT s) -> CDouble -> CDouble -> IO #{type cairo_bool_t}

cairoFill :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoFill = (`withCairoT` c_cairo_fill)

foreign import ccall "cairo_fill" c_cairo_fill :: Ptr (CairoT s) -> IO ()

cairoFillPreserve :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoFillPreserve = (`withCairoT` c_cairo_fill_preserve)

foreign import ccall "cairo_fill_preserve" c_cairo_fill_preserve :: Ptr (CairoT s) -> IO ()

cairoFillExtents :: PrimMonad m => CairoT (PrimState m) -> m CairoExtents
cairoFillExtents = flip withCairoT \pcr -> alloca \x1 -> alloca \y1 -> alloca \x2 -> alloca \y2 -> do
	c_cairo_fill_extents pcr x1 y1 x2 y2
	CairoExtentsLeftTopRightBottom <$> peek x1 <*> peek y1 <*> peek x2 <*> peek y2

foreign import ccall "cairo_fill_extents" c_cairo_fill_extents ::
	Ptr (CairoT s) -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()

cairoInFill :: PrimMonad m => CairoT (PrimState m) -> CDouble -> CDouble -> m Bool
cairoInFill cr x y = withCairoT cr \pcr -> (/= 0) <$> c_cairo_in_fill pcr x y

foreign import ccall "cairo_in_fill" c_cairo_in_fill ::
	Ptr (CairoT s) -> CDouble -> CDouble -> IO #{type cairo_bool_t}

cairoPaint :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoPaint cr = withCairoT cr \pcr -> c_cairo_paint pcr >> raiseIfError cr

foreign import ccall "cairo_paint" c_cairo_paint :: Ptr (CairoT s) -> IO ()

cairoPaintWithAlpha :: PrimMonad m => CairoT (PrimState m) -> CDouble -> m ()
cairoPaintWithAlpha cr a = withCairoT cr \pcr -> c_cairo_paint_with_alpha pcr a

foreign import ccall "cairo_paint_with_alpha" c_cairo_paint_with_alpha ::
	Ptr (CairoT s) -> CDouble -> IO ()

cairoMask :: PrimMonad m => CairoT s -> CairoPatternT s -> m ()
cairoMask (CairoT fcr) pt@(CairoPatternT fpt) = unsafeIOToPrim
	$ withForeignPtr fcr \pcr -> withForeignPtr fpt (c_cairo_mask pcr) >> raiseIfErrorPattern pt

foreign import ccall "cairo_mask" c_cairo_mask ::
	Ptr (CairoT s) -> Ptr (CairoPatternT s) -> IO ()

cairoMaskSurface :: PrimMonad m => CairoT s -> CairoSurfaceT s -> CDouble -> CDouble -> m ()
cairoMaskSurface (CairoT fcr) (CairoSurfaceT fsr) x y = unsafeIOToPrim
	$ withForeignPtr fcr \cr -> withForeignPtr fsr \sr -> c_cairo_mask_surface cr sr x y

foreign import ccall "cairo_mask_surface" c_cairo_mask_surface ::
	Ptr (CairoT s) -> Ptr (CairoSurfaceT s) -> CDouble -> CDouble -> IO ()
