{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.SvgSurfaces.Template where

import Language.Haskell.TH
import Data.Word
import Graphics.Cairo.Template

#include <cairo-svg.h>

newtype CairoSvgUnitT = CairoSvgUnitT #{type cairo_svg_unit_t} deriving Show

mkMember :: String -> Integer -> DecsQ
mkMember = mkMemberGen ''CairoSvgUnitT 'CairoSvgUnitT
