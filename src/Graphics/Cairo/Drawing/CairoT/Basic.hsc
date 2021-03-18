{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT.Basic where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Primitive
import Graphics.Cairo.Types

import Graphics.Cairo.Exception
import Graphics.Cairo.Monad

foreign import ccall "cairo_create" c_cairo_create :: Ptr (CairoSurfaceT s) -> IO (Ptr (CairoT s))

cairoCreate :: PrimMonad m => CairoSurfaceT (PrimState m) -> m (CairoT (PrimState m))
cairoCreate (CairoSurfaceT fs) = unPrimIo do
	cr <- makeCairoT =<< withForeignPtr fs c_cairo_create
	cr <$ raiseIfError cr
