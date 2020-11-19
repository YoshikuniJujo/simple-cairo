{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Graphics.Cairo.Values
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Monad

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 500 500
	cr <- cairoCreate s
	cairoLineTo cr 100 100
	cairoNewPath cr
	cairoLineTo cr 0 0
	cairoStroke cr
	cairoPopGroupToSource cr
	print =<< cairoImageSurfaceGetData s
