{-# LANGUAGE BlockArguments #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT.Setting where

import Foreign.Ptr
import Foreign.C.Types
import Control.Monad.Primitive
import Data.CairoContext

cairoSetLineWidth :: PrimMonad m => CairoT (PrimState m) -> CDouble -> m ()
cairoSetLineWidth cr w = withCairoT cr \pcr -> c_cairo_set_line_width pcr w

foreign import ccall "cairo_set_line_width" c_cairo_set_line_width ::
	Ptr (CairoT s) -> CDouble -> IO ()
