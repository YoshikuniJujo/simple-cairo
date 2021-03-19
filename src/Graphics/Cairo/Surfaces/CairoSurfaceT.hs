{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.CairoSurfaceT where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Control.Monad.Primitive

newtype CairoSurfaceT s = CairoSurfaceT (ForeignPtr (CairoSurfaceT s)) deriving Show

makeCairoSurfaceT :: Ptr (CairoSurfaceT s) -> IO (CairoSurfaceT s)
makeCairoSurfaceT p = CairoSurfaceT <$> newForeignPtr p (c_cairo_surface_destroy p)

makeCairoSurfaceT' :: Ptr (CairoSurfaceT s) -> Ptr a -> IO (CairoSurfaceT s)
makeCairoSurfaceT' ps p = CairoSurfaceT <$> newForeignPtr ps (free p >> c_cairo_surface_destroy ps)

foreign import ccall "cairo_surface_destroy" c_cairo_surface_destroy ::
	Ptr (CairoSurfaceT s) -> IO ()

cairoSurfaceFinish :: PrimMonad m => CairoSurfaceT (PrimState m) -> m ()
cairoSurfaceFinish (CairoSurfaceT fsr) = unsafeIOToPrim $ withForeignPtr fsr c_cairo_surface_finish

foreign import ccall "cairo_surface_finish" c_cairo_surface_finish ::
	Ptr (CairoSurfaceT s) -> IO ()

cairoSurfaceFlush :: PrimMonad m => CairoSurfaceT (PrimState m) -> m ()
cairoSurfaceFlush (CairoSurfaceT fsr) = unsafeIOToPrim $ withForeignPtr fsr c_cairo_surface_flush

foreign import ccall "cairo_surface_flush" c_cairo_surface_flush ::
	Ptr (CairoSurfaceT s) -> IO ()
