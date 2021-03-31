{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoPatternT.Basic where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Monad.Primitive
import Data.Word
import Data.Color
import System.IO.Unsafe

import Graphics.Cairo.Exception

#include <cairo.h>

class IsCairoPatternT pt where toCairoPatternT :: pt s -> CairoPatternT s

newtype CairoPatternT s = CairoPatternT (ForeignPtr (CairoPatternT s)) deriving Show

instance IsCairoPatternT CairoPatternT where toCairoPatternT = id

makeCairoPatternT :: Ptr (CairoPatternT s) -> IO (CairoPatternT s)
makeCairoPatternT p = CairoPatternT <$> newForeignPtr p (c_cairo_pattern_destroy p)

returnCairoPatternT :: PrimMonad m => IO (Ptr (CairoPatternT (PrimState m))) -> m (CairoPatternT (PrimState m))
returnCairoPatternT io = unsafeIOToPrim $ makeCairoPatternT =<< io

foreign import ccall "cairo_pattern_destroy" c_cairo_pattern_destroy ::
	Ptr (CairoPatternT s) -> IO ()

newtype CairoPatternTypeT = CairoPatternTypeT #{type cairo_pattern_type_t} deriving Show

pattern CairoPatternTypeSolid :: CairoPatternTypeT
pattern CairoPatternTypeSolid <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_SOLID} where
	CairoPatternTypeSolid = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_SOLID}

pattern CairoPatternTypeSurface :: CairoPatternTypeT
pattern CairoPatternTypeSurface <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_SURFACE} where
	CairoPatternTypeSurface = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_SURFACE}

pattern CairoPatternTypeLinear :: CairoPatternTypeT
pattern CairoPatternTypeLinear <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_LINEAR} where
	CairoPatternTypeLinear = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_LINEAR}

pattern CairoPatternTypeRadial :: CairoPatternTypeT
pattern CairoPatternTypeRadial <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_RADIAL} where
	CairoPatternTypeRadial = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_RADIAL}

pattern CairoPatternTypeMesh :: CairoPatternTypeT
pattern CairoPatternTypeMesh <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_MESH} where
	CairoPatternTypeMesh = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_MESH}

pattern CairoPatternTypeRasterSource :: CairoPatternTypeT
pattern CairoPatternTypeRasterSource <- CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_RASTER_SOURCE} where
	CairoPatternTypeRasterSource = CairoPatternTypeT #{const CAIRO_PATTERN_TYPE_RASTER_SOURCE}

cairoPatternGetType :: CairoPatternT s -> CairoPatternTypeT
cairoPatternGetType (CairoPatternT fpt) = unsafePerformIO $ withForeignPtr fpt \pt ->
	CairoPatternTypeT <$> c_cairo_pattern_get_type pt

foreign import ccall "cairo_pattern_get_type" c_cairo_pattern_get_type ::
	Ptr (CairoPatternT s) -> IO #{type cairo_pattern_type_t}

newtype CairoPatternSolidT s = CairoPatternSolidT (ForeignPtr (CairoPatternT s)) deriving Show

pattern CairoPatternTSolid :: CairoPatternSolidT s -> CairoPatternT s
pattern CairoPatternTSolid pts <- (cairoPatternSolidT -> Just pts) where
	CairoPatternTSolid (CairoPatternSolidT pts) = CairoPatternT pts

cairoPatternSolidT :: CairoPatternT s -> Maybe (CairoPatternSolidT s)
cairoPatternSolidT pt@(CairoPatternT fpt) = case cairoPatternGetType pt of
	CairoPatternTypeSolid -> Just $ CairoPatternSolidT fpt
	_ -> Nothing

cairoPatternCreateRgb :: PrimMonad m => Rgb -> m (CairoPatternSolidT (PrimState m))
cairoPatternCreateRgb (RgbDouble r g b) = unsafeIOToPrim do
	ppt <- c_cairo_pattern_create_rgb r g b
	pt <- CairoPatternSolidT <$> newForeignPtr ppt (c_cairo_pattern_destroy ppt)
	pt <$ raiseIfErrorPattern (CairoPatternTSolid pt)

foreign import ccall "cairo_pattern_create_rgb" c_cairo_pattern_create_rgb ::
	CDouble -> CDouble -> CDouble -> IO (Ptr (CairoPatternT s))

cairoPatternCreateRgba :: PrimMonad m => Rgba -> m (CairoPatternSolidT (PrimState m))
cairoPatternCreateRgba (RgbaDouble r g b a) = unsafeIOToPrim do
	ppt <- c_cairo_pattern_create_rgba r g b a
	pt <- CairoPatternSolidT <$> newForeignPtr ppt (c_cairo_pattern_destroy ppt)
	pt <$ raiseIfErrorPattern (CairoPatternTSolid pt)

foreign import ccall "cairo_pattern_create_rgba" c_cairo_pattern_create_rgba ::
	CDouble -> CDouble -> CDouble -> CDouble -> IO (Ptr (CairoPatternT s))

cairoPatternGetRgba :: PrimMonad m => CairoPatternSolidT (PrimState m) -> m Rgba
cairoPatternGetRgba (CairoPatternSolidT fpts) = unsafeIOToPrim $ withForeignPtr fpts \pts ->
	alloca \r -> alloca \g -> alloca \b -> alloca \a -> do
		cairoStatusToThrowError =<< c_cairo_pattern_get_rgba pts r g b a
		rgbaDouble <$> peek r <*> peek g <*> peek b <*> peek a >>= \case
			Just rgba -> pure rgba
			Nothing -> error $ "(r, g, b, a) = " ++ show (r, g, b, a)

foreign import ccall "cairo_pattern_get_rgba" c_cairo_pattern_get_rgba ::
	Ptr (CairoPatternT s) ->
	Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
	IO #{type cairo_status_t}

newtype CairoPatternGradientT s = CairoPatternGradientT (ForeignPtr (CairoPatternT s)) deriving Show

pattern CairoPatternTGradient :: CairoPatternGradientT s -> CairoPatternT s
pattern CairoPatternTGradient ptg <- (cairoPatternGradientT -> Just ptg) where
	CairoPatternTGradient (CairoPatternGradientT ptg) = CairoPatternT ptg

cairoPatternGradientT :: CairoPatternT s -> Maybe (CairoPatternGradientT s)
cairoPatternGradientT pt@(CairoPatternT fpt) = case cairoPatternGetType pt of
	CairoPatternTypeLinear -> Just $ CairoPatternGradientT fpt
	CairoPatternTypeRadial -> Just $ CairoPatternGradientT fpt
	_ -> Nothing

newtype CairoPatternLinearT s = CairoPatternLinearT (ForeignPtr (CairoPatternT s)) deriving Show

pattern CairoPatternGradientTLinear :: CairoPatternLinearT s -> CairoPatternGradientT s
pattern CairoPatternGradientTLinear ptl <- (cairoPatternGradientLinearT -> Just ptl) where
	CairoPatternGradientTLinear (CairoPatternLinearT ptl) = CairoPatternGradientT ptl

cairoPatternGradientLinearT :: CairoPatternGradientT s -> Maybe (CairoPatternLinearT s)
cairoPatternGradientLinearT pt@(CairoPatternGradientT fpt) = case cairoPatternGetType $ CairoPatternTGradient pt of
	CairoPatternTypeLinear -> Just $ CairoPatternLinearT fpt
	_ -> Nothing

raiseIfErrorPattern :: CairoPatternT s -> IO ()
raiseIfErrorPattern (CairoPatternT fpt) = withForeignPtr fpt \pt ->
	cairoStatusToThrowError =<< c_cairo_pattern_status pt

foreign import ccall "cairo_pattern_status" c_cairo_pattern_status ::
	Ptr (CairoPatternT s) -> IO #{type cairo_status_t}
