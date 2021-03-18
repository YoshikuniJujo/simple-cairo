{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Types where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Data.Int

#include <cairo.h>

newtype CairoSurfaceT s = CairoSurfaceT (ForeignPtr (CairoSurfaceT s)) deriving Show

makeCairoSurfaceT :: Ptr (CairoSurfaceT s) -> IO (CairoSurfaceT s)
makeCairoSurfaceT p = CairoSurfaceT <$> newForeignPtr p (c_cairo_surface_destroy p)

makeCairoSurfaceT' :: Ptr (CairoSurfaceT s) -> Ptr a -> IO (CairoSurfaceT s)
makeCairoSurfaceT' ps p = CairoSurfaceT <$> newForeignPtr ps (free p >> c_cairo_surface_destroy ps)

foreign import ccall "cairo_surface_destroy" c_cairo_surface_destroy ::
	Ptr (CairoSurfaceT s) -> IO ()

newtype CairoPatternT s = CairoPatternT (ForeignPtr (CairoPatternT s)) deriving Show

makeCairoPatternT :: Ptr (CairoPatternT s) -> IO (CairoPatternT s)
makeCairoPatternT p = CairoPatternT <$> newForeignPtr p (c_cairo_pattern_destroy p)

foreign import ccall "cairo_pattern_destroy" c_cairo_pattern_destroy ::
	Ptr (CairoPatternT s) -> IO ()

data CairoTextExtentsT = CairoTextExtentsT {
	cairoTextExtentsTXBearing :: #{type double},
	cairoTextExtentsTYBearing :: #{type double},
	cairoTextExtentsTWidth :: #{type double},
	cairoTextExtentsTHeight :: #{type double},
	cairoTextExtentsTXAdvance :: #{type double},
	cairoTextExtentsTYAdvance :: #{type double} } deriving Show

instance Storable CairoTextExtentsT where
	sizeOf _ = #{size cairo_text_extents_t}
	alignment _ = #{alignment cairo_text_extents_t}
	peek p = CairoTextExtentsT
		<$> #{peek cairo_text_extents_t, x_bearing} p
		<*> #{peek cairo_text_extents_t, y_bearing} p
		<*> #{peek cairo_text_extents_t, width} p
		<*> #{peek cairo_text_extents_t, height} p
		<*> #{peek cairo_text_extents_t, x_advance} p
		<*> #{peek cairo_text_extents_t, y_advance} p
	poke p (CairoTextExtentsT xb yb w h xa ya) = do
		#{poke cairo_text_extents_t, x_bearing} p xb
		#{poke cairo_text_extents_t, y_bearing} p yb
		#{poke cairo_text_extents_t, width} p w
		#{poke cairo_text_extents_t, height} p h
		#{poke cairo_text_extents_t, x_advance} p xa
		#{poke cairo_text_extents_t, y_advance} p ya

newtype CairoRegionT s = CairoRegionT (ForeignPtr (CairoRegionT s)) deriving Show

makeCairoRegionT :: Ptr (CairoRegionT s) -> IO (CairoRegionT s)
makeCairoRegionT p = CairoRegionT <$> newForeignPtr p (c_cairo_region_destroy p)

foreign import ccall "cairo_region_destroy" c_cairo_region_destroy ::
	Ptr (CairoRegionT s) -> IO ()

data CairoRectangleIntT = CairoRectangleIntT {
	cairoRectangleIntTX, cairoRectangleIntTY :: #{type int},
	cairoREctangleIntTWidth, cairoRectangleIntTHeight :: #{type int} }
	deriving Show

instance Storable CairoRectangleIntT where
	sizeOf _ = #size cairo_rectangle_int_t
	alignment _ = #alignment cairo_rectangle_int_t
	peek p = CairoRectangleIntT
		<$> #{peek cairo_rectangle_int_t, x} p
		<*> #{peek cairo_rectangle_int_t, y} p
		<*> #{peek cairo_rectangle_int_t, width} p
		<*> #{peek cairo_rectangle_int_t, height} p
	poke p (CairoRectangleIntT x y w h) = do
		#{poke cairo_rectangle_int_t, x} p x
		#{poke cairo_rectangle_int_t, y} p y
		#{poke cairo_rectangle_int_t, width} p w
		#{poke cairo_rectangle_int_t, height} p h
