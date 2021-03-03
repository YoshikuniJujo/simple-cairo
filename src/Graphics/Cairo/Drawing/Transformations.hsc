{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Transformations where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Primitive

import Graphics.Cairo.Monad
import Graphics.Cairo.Types

foreign import ccall "cairo_translate" c_cairo_translate :: Ptr (CairoT s) -> #{type double} -> #{type double} -> IO ()

cairoTranslate :: PrimMonad m => CairoT (PrimState m) -> #{type double} -> #{type double} -> m ()
cairoTranslate (CairoT fcr) tx ty = unPrimIo $ withForeignPtr fcr \cr -> c_cairo_translate cr tx ty

foreign import ccall "cairo_scale" c_cairo_scale :: Ptr (CairoT s) -> #{type double} -> #{type double} -> IO ()

cairoScale :: PrimMonad m => CairoT (PrimState m) -> #{type double} -> #{type double} -> m ()
cairoScale (CairoT fcr) sx sy = unPrimIo $ withForeignPtr fcr \cr -> c_cairo_scale cr sx sy

foreign import ccall "cairo_rotate" c_cairo_rotate :: Ptr (CairoT s) -> #{type double} -> IO ()

cairoRotate :: PrimMonad m => CairoT (PrimState m) -> #{type double} -> m ()
cairoRotate (CairoT fcr) a = unPrimIo $ withForeignPtr fcr \cr -> c_cairo_rotate cr a

foreign import ccall "cairo_identity_matrix" c_cairo_identity_matrix :: Ptr (CairoT s) -> IO ()

cairoIdentityMatrix :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoIdentityMatrix (CairoT fcr) = unPrimIo $ withForeignPtr fcr c_cairo_identity_matrix
