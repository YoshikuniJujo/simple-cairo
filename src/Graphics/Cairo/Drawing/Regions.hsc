{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Regions where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Primitive

import Graphics.Cairo.Exception
import Graphics.Cairo.Monad
import Graphics.Cairo.Types

#include <cairo.h>

foreign import ccall "cairo_region_create" c_cairo_region_create :: IO (Ptr (CairoRegionT s))

cairoRegionCreate :: PrimMonad m => m (CairoRegionT (PrimState m))
cairoRegionCreate = unPrimIo do
	r <- makeCairoRegionT =<< c_cairo_region_create
	r <$ raiseIfErrorRegion r

foreign import ccall "cairo_region_create_rectangle" c_cairo_region_create_rectangle ::
	Ptr CairoRectangleIntT -> IO (Ptr (CairoRegionT s))

cairoRegionCreateRectangle :: PrimMonad m => CairoRectangleIntT -> m (CairoRegionT (PrimState m))
cairoRegionCreateRectangle rct = unPrimIo $ alloca \prct -> do
	poke prct rct
	r <- makeCairoRegionT =<< c_cairo_region_create_rectangle prct
	r <$ raiseIfErrorRegion r
