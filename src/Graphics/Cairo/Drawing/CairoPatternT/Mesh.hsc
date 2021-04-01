{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoPatternT.Mesh where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Monad.Primitive
import Data.Word
import Data.Color

import Graphics.Cairo.Exception
import Graphics.Cairo.Drawing.Paths.CairoPathT (CairoPathT, mkCairoPathT)

#include <cairo.h>

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

cairoMeshPatternAddPatch :: PrimMonad m => CairoPatternMeshT (PrimState m) ->
	MoveTo -> LineCurveTo -> LineCurveTo -> LineCurveTo -> CloseTo ->
	Color -> Color -> Color -> Color ->
	Maybe Point -> Maybe Point -> Maybe Point -> Maybe Point -> m ()
cairoMeshPatternAddPatch pt mv lc1 lc2 lc3 cls c0 c1 c2 c3 cp0 cp1 cp2 cp3 = do
	cairoMeshPatternBeginPatch pt
	cairoMeshPatternMoveTo pt mv
	cairoMeshPatternLineTo pt lc1
	cairoMeshPatternLineTo pt lc2
	cairoMeshPatternLineTo pt lc3
	cairoMeshPatternCloseTo pt mv cls
	cairoMeshPatternSetCornerColor pt 0 c0
	cairoMeshPatternSetCornerColor pt 1 c1
	cairoMeshPatternSetCornerColor pt 2 c2
	cairoMeshPatternSetCornerColor pt 3 c3
	cairoMeshPatternSetControlPointMaybe pt 0 cp0
	cairoMeshPatternSetControlPointMaybe pt 1 cp1
	cairoMeshPatternSetControlPointMaybe pt 2 cp2
	cairoMeshPatternSetControlPointMaybe pt 3 cp3
	cairoMeshPatternEndPatch pt

cairoMeshPatternBeginPatch :: PrimMonad m => CairoPatternMeshT (PrimState m) -> m ()
cairoMeshPatternBeginPatch (CairoPatternMeshT fpt) = unsafeIOToPrim 
	$ withForeignPtr fpt c_cairo_mesh_pattern_begin_patch

foreign import ccall "cairo_mesh_pattern_begin_patch" c_cairo_mesh_pattern_begin_patch ::
	Ptr (CairoPatternT s) -> IO ()

cairoMeshPatternEndPatch :: PrimMonad m => CairoPatternMeshT (PrimState m) -> m ()
cairoMeshPatternEndPatch (CairoPatternMeshT fpt) = unsafeIOToPrim
	$ withForeignPtr fpt c_cairo_mesh_pattern_end_patch

foreign import ccall "cairo_mesh_pattern_end_patch" c_cairo_mesh_pattern_end_patch ::
	Ptr (CairoPatternT s) -> IO ()

cairoMeshPatternMoveTo :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) -> MoveTo -> m ()
cairoMeshPatternMoveTo (CairoPatternMeshT fpt) (MoveTo x y) = unsafeIOToPrim
	$ withForeignPtr fpt \ppt -> c_cairo_mesh_pattern_move_to ppt x y

foreign import ccall "cairo_mesh_pattern_move_to" c_cairo_mesh_pattern_move_to ::
	Ptr (CairoPatternT s) -> CDouble -> CDouble -> IO ()

cairoMeshPatternLineTo :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) -> LineCurveTo -> m ()
cairoMeshPatternLineTo (CairoPatternMeshT fpt) = \case
	LineTo x y -> unsafeIOToPrim
		$ withForeignPtr fpt \ppt -> c_cairo_mesh_pattern_line_to ppt x y
	CurveTo x1  y1 x2 y2 x3 y3 -> unsafeIOToPrim
		$ withForeignPtr fpt \ppt -> c_cairo_mesh_pattern_curve_to ppt x1 y1 x2 y2 x3 y3

cairoMeshPatternCloseTo :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) -> MoveTo -> CloseTo -> m ()
cairoMeshPatternCloseTo (CairoPatternMeshT fpt) (MoveTo x0 y0) cls =
	unsafeIOToPrim $ withForeignPtr fpt \ppt -> case cls of
		CloseLineTo -> pure ()
		CloseCurveTo x1 y1 x2 y2 -> c_cairo_mesh_pattern_curve_to ppt x1 y1 x2 y2 x0 y0

foreign import ccall "cairo_mesh_pattern_line_to" c_cairo_mesh_pattern_line_to ::
	Ptr (CairoPatternT s) -> CDouble -> CDouble -> IO ()

foreign import ccall "cairo_mesh_pattern_curve_to" c_cairo_mesh_pattern_curve_to ::
	Ptr (CairoPatternT s) ->
	CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

cairoMeshPatternSetControlPointMaybe :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) -> CUInt -> Maybe Point -> m ()
cairoMeshPatternSetControlPointMaybe (CairoPatternMeshT fpt) i = \case
	Nothing -> pure ()
	Just (Point x y) -> unsafeIOToPrim
		$ withForeignPtr fpt \ppt -> c_cairo_mesh_pattern_set_control_point ppt i x y

foreign import ccall "cairo_mesh_pattern_set_control_point" c_cairo_mesh_pattern_set_control_point ::
	Ptr (CairoPatternT s) -> CUInt -> CDouble -> CDouble -> IO ()

cairoMeshPatternSetCornerColor :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) -> CUInt -> Color -> m ()
cairoMeshPatternSetCornerColor (CairoPatternMeshT fpt) i c =
	unsafeIOToPrim $ withForeignPtr fpt \ppt -> case c of
		ColorRgb (RgbDouble r g b) -> c_cairo_mesh_pattern_set_corner_color_rgb ppt i r g b
		ColorRgba (RgbaDouble r g b a) -> c_cairo_mesh_pattern_set_corner_color_rgba ppt i r g b a

foreign import ccall "cairo_mesh_pattern_set_corner_color_rgb" c_cairo_mesh_pattern_set_corner_color_rgb ::
	Ptr (CairoPatternT s) -> CUInt -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall "cairo_mesh_pattern_set_corner_color_rgba" c_cairo_mesh_pattern_set_corner_color_rgba ::
	Ptr (CairoPatternT s) -> CUInt -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

cairoMeshPatternGetPatchCount :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) -> m CUInt
cairoMeshPatternGetPatchCount (CairoPatternMeshT fpt) =
	unsafeIOToPrim $ withForeignPtr fpt \ppt -> alloca \cnt -> do
		cairoStatusToThrowError =<< c_cairo_mesh_pattern_get_patch_count ppt cnt
		peek cnt

foreign import ccall "cairo_mesh_pattern_get_patch_count" c_cairo_mesh_pattern_get_patch_count ::
	Ptr (CairoPatternT s) -> Ptr CUInt -> IO #{type cairo_status_t}

cairoMeshPatternGetPath :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) -> CUInt -> m CairoPathT
cairoMeshPatternGetPath (CairoPatternMeshT fpt) i =
	unsafeIOToPrim $ withForeignPtr fpt \ppt ->
		mkCairoPathT =<< c_cairo_mesh_pattern_get_path ppt i

foreign import ccall "cairo_mesh_pattern_get_path" c_cairo_mesh_pattern_get_path ::
	Ptr (CairoPatternT s) -> CUInt -> IO (Ptr CairoPathT)

cairoMeshPatternGetControlPoint :: PrimMonad m =>
	CairoPatternMeshT (PrimState m) -> CUInt -> CUInt -> m (CDouble, CDouble)
cairoMeshPatternGetControlPoint (CairoPatternMeshT fpt) i j =
	unsafeIOToPrim $ withForeignPtr fpt \ppt -> alloca \x -> alloca \y -> do
		cairoStatusToThrowError =<< c_cairo_mesh_pattern_get_control_point ppt i j x y
		(,) <$> peek x <*> peek y

foreign import ccall "cairo_mesh_pattern_get_control_point" c_cairo_mesh_pattern_get_control_point ::
	Ptr (CairoPatternT s) -> CUInt -> CUInt -> Ptr CDouble -> Ptr CDouble -> IO #{type cairo_status_t}
