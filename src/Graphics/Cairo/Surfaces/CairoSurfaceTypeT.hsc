{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.CairoSurfaceTypeT (
	CairoSurfaceTypeT,
	module Graphics.Cairo.Surfaces.CairoSurfaceTypeT
	) where

import Graphics.Cairo.Surfaces.CairoSurfaceTypeT.Template

#include <cairo.h>

mkMember "CairoSurfaceTypeImage" #{const CAIRO_SURFACE_TYPE_IMAGE}
mkMember "CairoSurfaceTypePdf" #{const CAIRO_SURFACE_TYPE_PDF}
