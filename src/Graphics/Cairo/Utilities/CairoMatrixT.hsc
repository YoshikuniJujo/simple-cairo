{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Utilities.CairoMatrixT where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Monad.Primitive

#include <cairo.h>

newtype CairoMatrixT s = CairoMatrixT (ForeignPtr (CairoMatrixT s)) deriving Show

data Matrix = Matrix CDouble CDouble CDouble CDouble CDouble CDouble deriving Show

cairoMatrixNew :: PrimMonad m => m (CairoMatrixT (PrimState m))
cairoMatrixNew = (CairoMatrixT <$>) $ unsafeIOToPrim
	$ ($) <$> newForeignPtr <*> free =<< mallocBytes #{size cairo_matrix_t}

cairoMatrixGet :: PrimMonad m => CairoMatrixT (PrimState m) -> m Matrix
cairoMatrixGet (CairoMatrixT f) =
	unsafeIOToPrim $ withForeignPtr f \p -> Matrix
		<$> #{peek cairo_matrix_t, xx} p <*> #{peek cairo_matrix_t, yx} p
		<*> #{peek cairo_matrix_t, yx} p <*> #{peek cairo_matrix_t, yy} p
		<*> #{peek cairo_matrix_t, x0} p <*> #{peek cairo_matrix_t, y0} p

cairoMatrixInit :: PrimMonad m => CairoMatrixT (PrimState m) ->
	CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> m ()
cairoMatrixInit (CairoMatrixT fmtx) xx yx xy yy x0 y0 =
	unsafeIOToPrim $ withForeignPtr fmtx \pmtx -> c_cairo_matrix_init pmtx xx yx xy yy x0 y0

foreign import ccall "cairo_matrix_init" c_cairo_matrix_init ::
	Ptr (CairoMatrixT s) -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

cairoMatrixInitIdentity :: PrimMonad m => CairoMatrixT (PrimState m) -> m ()
cairoMatrixInitIdentity (CairoMatrixT fmtx) =
	unsafeIOToPrim $ withForeignPtr fmtx c_cairo_matrix_init_identity

foreign import ccall "cairo_matrix_init_identity" c_cairo_matrix_init_identity ::
	Ptr (CairoMatrixT s) -> IO ()

cairoMatrixInitTranslate :: PrimMonad m => CairoMatrixT (PrimState m) ->
	CDouble -> CDouble -> m ()
cairoMatrixInitTranslate (CairoMatrixT fmtx) tx ty =
	unsafeIOToPrim $ withForeignPtr fmtx \pmtx -> c_cairo_matrix_init_translate pmtx tx ty

foreign import ccall "cairo_matrix_init_translate" c_cairo_matrix_init_translate ::
	Ptr (CairoMatrixT s) -> CDouble -> CDouble -> IO ()

cairoMatrixInitScale :: PrimMonad m => CairoMatrixT (PrimState m) ->
	CDouble -> CDouble -> m ()
cairoMatrixInitScale (CairoMatrixT fmtx) sx sy =
	unsafeIOToPrim $ withForeignPtr fmtx \pmtx -> c_cairo_matrix_init_scale pmtx sx sy

foreign import ccall "cairo_matrix_init_scale" c_cairo_matrix_init_scale ::
	Ptr (CairoMatrixT s) -> CDouble -> CDouble -> IO ()

cairoMatrixInitRotate :: PrimMonad m => CairoMatrixT (PrimState m) -> CDouble -> m ()
cairoMatrixInitRotate (CairoMatrixT fmtx) rad =
	unsafeIOToPrim $ withForeignPtr fmtx \pmtx -> c_cairo_matrix_init_rotate pmtx rad

foreign import ccall "cairo_matrix_init_rotate" c_cairo_matrix_init_rotate ::
	Ptr (CairoMatrixT s) -> CDouble -> IO ()

cairoMatrixTranslate :: PrimMonad m => CairoMatrixT (PrimState m) ->
	CDouble -> CDouble -> m ()
cairoMatrixTranslate (CairoMatrixT fmtx) tx ty =
	unsafeIOToPrim $ withForeignPtr fmtx \pmtx -> c_cairo_matrix_translate pmtx tx ty

foreign import ccall "cairo_matrix_translate" c_cairo_matrix_translate ::
	Ptr (CairoMatrixT s) -> CDouble -> CDouble -> IO ()
