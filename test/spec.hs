{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad.ST
import Data.Vector.Storable
import Data.Word
import Data.Int
import Graphics.Cairo.CairoT
import Graphics.Cairo.Paths
import Graphics.Cairo.ImageSurfaces
import Graphics.Cairo.Monad
import Graphics.Cairo.Types
import Graphics.Cairo.Values

import Codec.Picture hiding (pixelAt, generateImage)

import Graphics.Cairo.CairoImage

main :: IO ()
main = do
	let	(i, f, s) = runST red
	print (f, s)
	putStrLn ""
	print =<< redIo
	print $ runST green
	drawBlue

red :: forall s . ST s (DynamicImage, CairoFormatT, Int32) -- (Vector Word8, CairoFormatT, Int32)
red = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 50 50
	cr <- cairoCreate s
	cairoSetSourceRgb cr 1 0 0
	cairoPaint cr
	(,,) <$> cairoImageSurfaceGetImage s <*> cairoImageSurfaceGetFormat s <*> cairoImageSurfaceGetStride s

redIo :: IO (Either String Bool) -- DynamicImage -- (Vector Word8)
redIo = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 50 50
	cr <- cairoCreate s
	cairoSetSourceRgb cr 1 0 0
	cairoRectangle cr 0 0 25 25
	cairoFill cr
	cairoSetSourceRgb cr 0 1 0
	cairoRectangle cr 25 0 25 25
	cairoFill cr
	cairoSetSourceRgb cr 0.5 0.5 1
--	cairoSetSourceRgb cr 1 0 0
	cairoRectangle cr 0 25 25 25
	cairoFill cr
	cairoSetSourceRgb cr 1 1 0
--	cairoSetSourceRgb cr 1 0 0
	cairoRectangle cr 25 25 25 25
	cairoFill cr
	writeDynamicPng "tmp.png" =<< cairoImageSurfaceGetImage s

green :: forall s . ST s (
	Either (Argb32, Maybe PixelArgb32, Maybe PixelArgb32, Maybe PixelArgb32) CairoImage,
	Maybe PixelArgb32 )
green = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 50 50
	cr <- cairoCreate s
	cairoSetSourceRgb cr 0 1 0
	cairoPaint cr
	r1 <- (<$> cairoImageSurfaceGetCairoImage s) \case
		CairoImageArgb32 a -> Left (a, pixelAt a 0 0, pixelAt a 30 30, pixelAt a 49 49)
		i -> Right i
	Left r2 <- (<$> cairoImageSurfaceGetCairoImageMut s) \case
		CairoImageMutArgb32 a -> Left a
		i -> Right i
	p2020 <- getPixel r2 20 20
	pure (r1, p2020)

blue :: Argb32
blue = generateImage 200 200 \_ _ -> PixelArgb32 0xff0000ff

blueSurface :: ST s (CairoSurfaceT s)
blueSurface = cairoImageSurfaceCreateForCairoImage $ CairoImageArgb32 blue

drawBlue :: IO ()
drawBlue = do
	writeDynamicPng "tmp2.png" $ runST (cairoImageSurfaceGetImage =<<  blueSurface)
	pure ()
