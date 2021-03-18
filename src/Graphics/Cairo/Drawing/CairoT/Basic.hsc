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

foreign import ccall "cairo_create" c_cairo_create :: Ptr (CairoSurfaceT s) -> IO (Ptr (CairoT s))

cairoCreate :: PrimMonad m => CairoSurfaceT (PrimState m) -> m (CairoT (PrimState m))
cairoCreate (CairoSurfaceT fs) = unsafeIOToPrim do
	cr <- makeCairoT =<< withForeignPtr fs c_cairo_create
	cr <$ raiseIfError cr

makeCairoT :: Ptr (CairoT s) -> IO (CairoT s)
makeCairoT p = CairoT <$> newForeignPtr p (c_cairo_destroy p)

foreign import ccall "cairo_destroy" c_cairo_destroy :: Ptr (CairoT s) -> IO ()
