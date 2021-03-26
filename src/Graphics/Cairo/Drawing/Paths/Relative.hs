{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Paths.Relative where

import Foreign.Ptr
import Foreign.C.Types
import Control.Monad.Primitive
import Data.CairoContext
import Graphics.Cairo.Exception

cairoRelMoveTo :: PrimMonad m => CairoT (PrimState m) -> CDouble -> CDouble -> m ()
cairoRelMoveTo cr dx dy = do
	withCairoT cr \pcr -> c_cairo_rel_move_to pcr dx dy
	unsafeIOToPrim $ raiseIfError cr

foreign import ccall "cairo_rel_move_to" c_cairo_rel_move_to ::
	Ptr (CairoT s) -> CDouble -> CDouble -> IO ()
