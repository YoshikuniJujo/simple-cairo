{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo (
	CairoT, CairoSurfaceT, CairoStatusT,
	cairoFill, cairoRectangle, cairoSetSourceRgba, cairoStroke, cairoSetLineWidth,
	cairoSetSourceRgb, cairoCreate, cairoFormatArgb32, cairoImageSurfaceCreate,
	cairoPaintWithAlpha, cairoPaint,

	) where

#include <cairo.h>

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Paths
import Graphics.Cairo.ImageSurfaces
import Graphics.Cairo.Exception
import Graphics.Cairo.Types
import Graphics.Cairo.Values
