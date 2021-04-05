{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Transformations where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Control.Monad.Primitive

import Data.CairoContext

import Graphics.Cairo.Utilities.CairoMatrixT.Internal

cairoTranslate :: PrimMonad m =>
	CairoT (PrimState m) -> CDouble -> CDouble -> m ()
cairoTranslate (CairoT fcr) tx ty =
	unsafeIOToPrim $ withForeignPtr fcr \cr -> c_cairo_translate cr tx ty

foreign import ccall "cairo_translate" c_cairo_translate ::
	Ptr (CairoT s) -> CDouble -> CDouble -> IO ()

cairoScale :: PrimMonad m =>
	CairoT (PrimState m) -> CDouble -> CDouble -> m ()
cairoScale (CairoT fcr) sx sy =
	unsafeIOToPrim $ withForeignPtr fcr \cr -> c_cairo_scale cr sx sy

foreign import ccall "cairo_scale" c_cairo_scale ::
	Ptr (CairoT s) -> CDouble -> CDouble -> IO ()

cairoRotate :: PrimMonad m => CairoT (PrimState m) -> CDouble -> m ()
cairoRotate (CairoT fcr) a =
	unsafeIOToPrim $ withForeignPtr fcr \cr -> c_cairo_rotate cr a

foreign import ccall "cairo_rotate" c_cairo_rotate ::
	Ptr (CairoT s) -> CDouble -> IO ()

cairoTransform :: (PrimMonad m, IsCairoMatrixT mtx) =>
	CairoT (PrimState m) -> mtx (PrimState m) -> m ()
cairoTransform (CairoT fcr) (toCairoMatrixT -> CairoMatrixT fmtx) =
	unsafeIOToPrim $ withForeignPtr fcr \pcr -> withForeignPtr fmtx \pmtx ->
		c_cairo_transform pcr pmtx

foreign import ccall "cairo_transform" c_cairo_transform ::
	Ptr (CairoT s) -> Ptr (CairoMatrixT s) -> IO ()

cairoIdentityMatrix :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoIdentityMatrix (CairoT fcr) =
	unsafeIOToPrim $ withForeignPtr fcr c_cairo_identity_matrix

foreign import ccall "cairo_identity_matrix" c_cairo_identity_matrix :: Ptr (CairoT s) -> IO ()
