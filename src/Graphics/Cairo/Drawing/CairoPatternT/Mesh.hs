{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoPatternT.Mesh where

import Foreign.ForeignPtr

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
