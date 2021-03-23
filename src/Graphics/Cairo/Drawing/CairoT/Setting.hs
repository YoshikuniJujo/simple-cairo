{-# LANGUAGE BlockArguments #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT.Setting where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.Types
import Control.Monad.Primitive
import Data.CairoContext
import Graphics.Cairo.Exception

cairoSetLineWidth :: PrimMonad m => CairoT (PrimState m) -> CDouble -> m ()
cairoSetLineWidth cr w = withCairoT cr \pcr -> c_cairo_set_line_width pcr w

foreign import ccall "cairo_set_line_width" c_cairo_set_line_width ::
	Ptr (CairoT s) -> CDouble -> IO ()

cairoSetDash :: PrimMonad m => CairoT (PrimState m) -> [CDouble] -> CDouble -> m ()
cairoSetDash cr ds ofs = withCairoT cr \pcr -> withArrayLen ds \lds pds -> do
	c_cairo_set_dash pcr pds (fromIntegral lds) ofs
	raiseIfError cr

foreign import ccall "cairo_set_dash" c_cairo_set_dash ::
	Ptr (CairoT s) -> Ptr CDouble -> CInt -> CDouble -> IO ()
