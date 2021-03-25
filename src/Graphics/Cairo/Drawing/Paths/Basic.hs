{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Paths.Basic where

import Foreign.Ptr
import Foreign.C.Types
import Control.Monad.Primitive
import Data.CairoContext

cairoNewPath :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoNewPath = (`withCairoT` c_cairo_new_path)

foreign import ccall "cairo_new_path" c_cairo_new_path :: Ptr (CairoT s) -> IO ()

cairoNewSubPath :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoNewSubPath = (`withCairoT` c_cairo_new_sub_path)

foreign import ccall "cairo_new_sub_path" c_cairo_new_sub_path :: Ptr (CairoT s) -> IO ()

cairoMoveTo :: PrimMonad m => CairoT (PrimState m) -> CDouble -> CDouble -> m ()
cairoMoveTo cr x y = withCairoT cr \pcr -> c_cairo_move_to pcr x y

foreign import ccall "cairo_move_to" c_cairo_move_to ::
	Ptr (CairoT s) -> CDouble -> CDouble -> IO ()

cairoLineTo :: PrimMonad m => CairoT (PrimState m) -> CDouble -> CDouble -> m ()
cairoLineTo cr x y = withCairoT cr \pcr -> c_cairo_line_to pcr x y

foreign import ccall "cairo_line_to" c_cairo_line_to ::
	Ptr (CairoT s) -> CDouble -> CDouble -> IO ()
