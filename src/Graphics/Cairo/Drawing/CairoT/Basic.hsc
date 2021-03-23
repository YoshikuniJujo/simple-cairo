{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT.Basic (
	cairoCreate,
	cairoSetSourceRgb, cairoSetSourceRgba, cairoSetSource, cairoSetSourceSurface,
	cairoStroke, cairoStrokePreserve,
	) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C.Types
import Control.Monad.Primitive

import Graphics.Cairo.Surfaces.CairoSurfaceT
import Graphics.Cairo.Exception

import Data.Color
import Data.CairoContext

import Graphics.Cairo.Types

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
cairoSetSource (CairoT fcr) (CairoPatternT fpt) = unsafeIOToPrim
	$ withForeignPtr fcr \cr -> withForeignPtr fpt \pt -> c_cairo_set_source cr pt

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
