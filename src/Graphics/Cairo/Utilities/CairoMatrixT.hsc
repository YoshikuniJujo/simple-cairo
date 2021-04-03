{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Utilities.CairoMatrixT where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Control.Monad.Primitive

newtype CairoMatrixT s = CairoMatrixT (ForeignPtr (CairoMatrixT s)) deriving Show

cairoMatrixInit :: PrimMonad m => CairoMatrixT (PrimState m) ->
	CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> m ()
cairoMatrixInit (CairoMatrixT fmtx) xx yx xy yy x0 y0 =
	unsafeIOToPrim $ withForeignPtr fmtx \pmtx -> c_cairo_matrix_init pmtx xx yx xy yy x0 y0

foreign import ccall "cairo_matrix_init" c_cairo_matrix_init ::
	Ptr (CairoMatrixT s) -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
