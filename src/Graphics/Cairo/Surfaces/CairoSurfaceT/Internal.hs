{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.CairoSurfaceT.Internal where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Control.Monad.Primitive

newtype CairoSurfaceT s = CairoSurfaceT (ForeignPtr (CairoSurfaceT s)) deriving Show

mkCairoSurfaceT :: Ptr (CairoSurfaceT s) -> IO (CairoSurfaceT s)
mkCairoSurfaceT p = CairoSurfaceT <$> newForeignPtr p (c_cairo_surface_destroy p)

mkCairoSurfaceT' :: Ptr (CairoSurfaceT s) -> Ptr a -> IO (CairoSurfaceT s)
mkCairoSurfaceT' ps p = CairoSurfaceT <$> newForeignPtr ps (free p >> c_cairo_surface_destroy ps)

foreign import ccall "cairo_surface_destroy" c_cairo_surface_destroy ::
	Ptr (CairoSurfaceT s) -> IO ()

foreign import ccall "cairo_surface_finish" c_cairo_surface_finish ::
	Ptr (CairoSurfaceT s) -> IO ()
