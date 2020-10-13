{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad.ST
import Data.Vector.Storable
import Data.Word
import Data.Int
import Graphics.Cairo.CairoT
import Graphics.Cairo.ImageSurfaces
import Graphics.Cairo.Monad
import Graphics.Cairo.Types
import Graphics.Cairo.Values

main :: IO ()
main = do
	print $ runST red
	putStrLn ""
	print =<< redIo

red :: forall s . ST s (Vector Word8, CairoFormatT, Int32)
red = do
	s <- cairoImageSurfaceCreate @s cairoFormatArgb32 500 500
	cr <- cairoCreate s
	cairoSetSourceRgb cr 1 0 0
	cairoPaint cr
	(,,) <$> cairoImageSurfaceGetData s <*> cairoImageSurfaceGetFormat s <*> cairoImageSurfaceGetStride s

redIo :: IO (Vector Word8)
redIo = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 500 500
	cr <- cairoCreate s
	cairoSetSourceRgb cr 1 0 0
	cairoPaint cr
	cairoImageSurfaceGetData s
