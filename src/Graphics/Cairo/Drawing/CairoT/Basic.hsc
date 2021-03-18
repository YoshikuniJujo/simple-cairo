{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT.Basic (
	cairoCreate,
	cairoSetSourceRgb
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

cairoCreate :: PrimMonad m =>
	CairoSurfaceT (PrimState m) -> m (CairoT (PrimState m))
cairoCreate (CairoSurfaceT sr) = unsafeIOToPrim do
	cr <- withForeignPtr sr c_cairo_create >>= \pcr ->
		CairoT <$> newForeignPtr pcr (c_cairo_destroy pcr)
	cr <$ raiseIfError cr

foreign import ccall "cairo_create"
	c_cairo_create :: Ptr (CairoSurfaceT s) -> IO (Ptr (CairoT s))
foreign import ccall "cairo_destroy" c_cairo_destroy :: Ptr (CairoT s) -> IO ()

foreign import ccall "cairo_set_source_rgb" c_cairo_set_source_rgb ::
	Ptr (CairoT s) -> CDouble -> CDouble -> CDouble -> IO ()

cairoSetSourceRgb :: PrimMonad m => CairoT (PrimState m) -> Rgb -> m ()
cairoSetSourceRgb cr (RgbDouble r g b) =
	argCairoT (\pcr -> c_cairo_set_source_rgb pcr r g b) cr
