{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoPatternT.Basic where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C.Types
import Control.Monad.Primitive

newtype CairoPatternT s = CairoPatternT (ForeignPtr (CairoPatternT s)) deriving Show

makeCairoPatternT :: Ptr (CairoPatternT s) -> IO (CairoPatternT s)
makeCairoPatternT p = CairoPatternT <$> newForeignPtr p (c_cairo_pattern_destroy p)

foreign import ccall "cairo_pattern_destroy" c_cairo_pattern_destroy ::
	Ptr (CairoPatternT s) -> IO ()

cairoPatternCreateRgb :: PrimMonad m =>
	CDouble -> CDouble -> CDouble -> m (CairoPatternT (PrimState m))
cairoPatternCreateRgb r g b = returnCairoPatternT
	$ c_cairo_pattern_create_rgb r g b

foreign import ccall "cairo_pattern_create_rgb" c_cairo_pattern_create_rgb ::
	CDouble -> CDouble -> CDouble -> IO (Ptr (CairoPatternT s))

returnCairoPatternT :: PrimMonad m => IO (Ptr (CairoPatternT (PrimState m))) -> m (CairoPatternT (PrimState m))
returnCairoPatternT io = unsafeIOToPrim $ makeCairoPatternT =<< io
