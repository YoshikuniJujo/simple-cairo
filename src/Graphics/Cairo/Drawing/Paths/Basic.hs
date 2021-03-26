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

cairoCurveTo :: PrimMonad m => CairoT (PrimState m) -> CDouble -> CDouble ->
	CDouble -> CDouble -> CDouble -> CDouble -> m ()
cairoCurveTo cr x1 y1 x2 y2 x3 y3 = withCairoT cr \pcr -> c_cairo_curve_to pcr x1 y1 x2 y2 x3 y3

foreign import ccall "cairo_curve_to" c_cairo_curve_to ::
	Ptr (CairoT s) -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

cairoClosePath :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoClosePath = (`withCairoT` c_cairo_close_path)

foreign import ccall "cairo_close_path" c_cairo_close_path ::
	Ptr (CairoT s) -> IO ()
