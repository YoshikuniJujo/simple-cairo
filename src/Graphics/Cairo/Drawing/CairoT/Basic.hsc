{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT.Basic (
	cairoCreate
	) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Control.Monad.Primitive

import Graphics.Cairo.Surfaces.CairoSurfaceT
import Graphics.Cairo.Exception

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
