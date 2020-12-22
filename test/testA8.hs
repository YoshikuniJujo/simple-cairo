{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Title

import Data.Word
import Data.Int
import Data.CairoImage.Internal
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Codec.Picture hiding (pixelAt, generateImage)

main :: IO ()
main = do
	putStrLn $ mkTitle "test a8"
	let	a8 = generateImage 33 33 (\x y -> PixelA8 $ circle x y) :: A8
	p <- cairoPatternCreateForSurface =<< cairoImageSurfaceCreateForCairoImage (CairoImageA8 a8)
	print p
	s <- cairoImageSurfaceCreate cairoFormatArgb32 33 33
	cr <- cairoCreate s
	cairoSetSourceRgb cr 0 0 0
	cairoMask cr p
	writeDynamicPng "testA8.png" =<< cairoImageSurfaceGetImage s
	pure ()

circle :: Int32 -> Int32 -> Word8
circle x_ y_ = round $ sqrt (x ^ (2 :: Int) + y ^ (2 :: Int))
	where
	x = (fromIntegral x_ - 16) * 0x100 / 16 :: Double
	y = (fromIntegral y_ - 16) * 0x100 / 16 :: Double
