{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.CairoSurfaceTypeT.Template where

import Language.Haskell.TH
import Data.Word

#include <cairo.h>

newtype CairoSurfaceTypeT = CairoSurfaceTypeT #{type cairo_surface_type_t} deriving Show

mkMember :: String -> Integer -> DecsQ
mkMember = mkMemberGen ''CairoSurfaceTypeT 'CairoSurfaceTypeT

mkMemberGen :: Name -> Name -> String -> Integer -> DecsQ
mkMemberGen t c n v = sequence [
	patSynSigD (mkName n) (conT t),
	patSynD (mkName n) (prefixPatSyn [])
		(explBidir [clause [] (normalB (conE c `appE` litE (IntegerL v))) []])
		(conP c [litP (IntegerL v)])
	]
