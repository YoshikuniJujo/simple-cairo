{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Regions where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Control.Monad.Primitive

import Graphics.Cairo.Exception
import Graphics.Cairo.Utilities.Types

#include <cairo.h>

newtype CairoRegionT s = CairoRegionT (ForeignPtr (CairoRegionT s)) deriving Show

makeCairoRegionT :: Ptr (CairoRegionT s) -> IO (CairoRegionT s)
makeCairoRegionT p = CairoRegionT <$> newForeignPtr p (c_cairo_region_destroy p)

foreign import ccall "cairo_region_destroy" c_cairo_region_destroy ::
	Ptr (CairoRegionT s) -> IO ()

foreign import ccall "cairo_region_create" c_cairo_region_create :: IO (Ptr (CairoRegionT s))

cairoRegionCreate :: PrimMonad m => m (CairoRegionT (PrimState m))
cairoRegionCreate = unsafeIOToPrim do
	r <- makeCairoRegionT =<< c_cairo_region_create
	r <$ raiseIfErrorRegion r

foreign import ccall "cairo_region_create_rectangle" c_cairo_region_create_rectangle ::
	Ptr CairoRectangleIntT -> IO (Ptr (CairoRegionT s))

cairoRegionCreateRectangle :: PrimMonad m => CairoRectangleIntT -> m (CairoRegionT (PrimState m))
cairoRegionCreateRectangle (CairoRectangleIntT_ fr) = unsafeIOToPrim $ withForeignPtr fr \prct -> do
	r <- makeCairoRegionT =<< c_cairo_region_create_rectangle prct
	r <$ raiseIfErrorRegion r
