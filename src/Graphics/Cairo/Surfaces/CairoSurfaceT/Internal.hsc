{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.CairoSurfaceT.Internal where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Control.Monad.Primitive
import Data.Word

import Graphics.Cairo.Surfaces.CairoSurfaceTypeT

#include <cairo.h>

newtype CairoSurfaceT s = CairoSurfaceT (ForeignPtr (CairoSurfaceT s)) deriving Show

mkCairoSurfaceT :: Ptr (CairoSurfaceT s) -> IO (CairoSurfaceT s)
mkCairoSurfaceT p = CairoSurfaceT <$> newForeignPtr p (c_cairo_surface_destroy p)

mkCairoSurfaceT' :: Ptr (CairoSurfaceT s) -> Ptr a -> IO (CairoSurfaceT s)
mkCairoSurfaceT' ps p = CairoSurfaceT <$> newForeignPtr ps (free p >> c_cairo_surface_destroy ps)

foreign import ccall "cairo_surface_destroy" c_cairo_surface_destroy ::
	Ptr (CairoSurfaceT s) -> IO ()

foreign import ccall "cairo_surface_finish" c_cairo_surface_finish ::
	Ptr (CairoSurfaceT s) -> IO ()

cairoSurfaceGetType :: PrimMonad m => CairoSurfaceT (PrimState m) -> m CairoSurfaceTypeT
cairoSurfaceGetType (CairoSurfaceT fsr) = unsafeIOToPrim
	$ CairoSurfaceTypeT <$> withForeignPtr fsr c_cairo_surface_get_type

foreign import ccall "cairo_surface_get_type" c_cairo_surface_get_type ::
	Ptr (CairoSurfaceT s) -> IO #{type cairo_surface_type_t}
