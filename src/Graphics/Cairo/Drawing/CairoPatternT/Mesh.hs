{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoPatternT.Mesh where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C.Types
import Control.Monad.Primitive
import Data.Color

import Graphics.Cairo.Drawing.CairoPatternT.Basic

newtype CairoPatternMeshT s = CairoPatternMeshT (ForeignPtr (CairoPatternT s)) deriving Show

instance IsCairoPatternT CairoPatternMeshT where
	toCairoPatternT = CairoPatternTMesh

pattern CairoPatternTMesh :: CairoPatternMeshT s -> CairoPatternT s
pattern CairoPatternTMesh pt <- (cairoPatternMeshT -> Just pt) where
	CairoPatternTMesh (CairoPatternMeshT fpt) = CairoPatternT fpt

cairoPatternMeshT :: CairoPatternT s -> Maybe (CairoPatternMeshT s)
cairoPatternMeshT pt@(CairoPatternT fpt) = case cairoPatternGetType pt of
	CairoPatternTypeMesh -> Just $ CairoPatternMeshT fpt
	_ -> Nothing

cairoPatternCreateMesh :: PrimMonad m => m (CairoPatternMeshT (PrimState m))
cairoPatternCreateMesh = unsafeIOToPrim do
	p <- c_cairo_pattern_create_mesh
	CairoPatternMeshT <$> newForeignPtr p (c_cairo_pattern_destroy p)

foreign import ccall "cairo_pattern_create_mesh" c_cairo_pattern_create_mesh ::
	IO (Ptr (CairoPatternT s))

data MoveTo = MoveTo CDouble CDouble deriving Show

data LineCurveTo
	= LineTo CDouble CDouble
	| CurveTo CDouble CDouble CDouble CDouble CDouble CDouble
	deriving Show

data CloseTo
	= CloseLineTo
	| CloseCurveTo CDouble CDouble CDouble CDouble
	deriving Show

data Color = ColorRgb Rgb | ColorRgba Rgba deriving Show

data Point = Point CDouble CDouble deriving Show
