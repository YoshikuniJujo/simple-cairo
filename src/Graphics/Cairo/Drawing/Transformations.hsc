{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Transformations where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Primitive

import Graphics.Cairo.Monad
import Graphics.Cairo.Types

foreign import ccall "cairo_scale" c_cairo_scale :: Ptr (CairoT s) -> #{type double} -> #{type double} -> IO ()

cairoScale :: PrimMonad m => CairoT (PrimState m) -> #{type double} -> #{type double} -> m ()
cairoScale (CairoT fcr) sx sy = unPrimIo $ withForeignPtr fcr \cr -> c_cairo_scale cr sx sy
