{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Graphics.Cairo.Exception
import Graphics.Cairo.Surfaces.PngSupport

import Paths_simple_cairo

main :: IO ()
main = do
	putStrLn "*** TEST ROTATE BEGIN ***"
	sfc <- cairoSurfaceCreateFromPng =<< getDataFileName "HaskellLogo.png"
	cairoStatusToThrowError . (\(CairoStatusT w) -> w) =<< cairoSurfaceWriteToPng sfc "HaskellLogoRotated.png"
	putStrLn "*** TEST ROTATE END ***"

