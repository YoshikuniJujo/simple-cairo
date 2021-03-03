{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.JuicyCairo (cairoArgb32ToJuicyRGBA8, juicyRGBA8ToCairoArgb32) where

import Control.Arrow
import Data.Maybe
import Data.Word

import qualified Data.CairoImage.Internal as C
import qualified Codec.Picture as J

cairoToJuicy :: (C.Image i, J.Pixel p) => (C.Pixel i -> p) -> i -> J.Image p
cairoToJuicy f i = (uncurry . J.generateImage)
	(\x y -> f . fromJust $ C.pixelAt i (fromIntegral x) (fromIntegral y))
	(fromIntegral *** fromIntegral $ C.imageSize i)

pixelArgb32ToPixelRGBA8 :: C.PixelArgb32 -> J.PixelRGBA8
pixelArgb32ToPixelRGBA8 (C.PixelArgb32 a r g b) = J.PixelRGBA8 r' g' b' a
	where
	r' = r `unit` (0xff, a)
	g' = g `unit` (0xff, a)
	b' = b `unit` (0xff, a)

unit :: Word8 -> (Word8, Word8) -> Word8
n `unit` (m, d) = fromIntegral
	(fromIntegral n * fromIntegral m `div'` fromIntegral d :: Word16)

infixl 7 `div'`

div' :: Integral n => n -> n -> n
_ `div'` 0 = 0
n `div'` m = n `div` m

cairoArgb32ToJuicyRGBA8 :: C.Argb32 -> J.Image J.PixelRGBA8
cairoArgb32ToJuicyRGBA8 = cairoToJuicy pixelArgb32ToPixelRGBA8

sample1 :: C.Argb32
sample1 = C.generateImage 256 256 \x y -> C.PixelArgb32
	255
	(fromIntegral x)
	(fromIntegral y)
	(fromIntegral $ 255 - x `div` 2 - y `div` 2)

sample1Png :: IO ()
sample1Png = J.writePng "sample1.png" $ cairoArgb32ToJuicyRGBA8 sample1

juicyToCairo :: (J.Pixel p, C.Image i) => (p -> C.Pixel i) -> J.Image p -> i
juicyToCairo f i = C.generateImage
	(fromIntegral $ J.imageWidth i)
	(fromIntegral $ J.imageHeight i)
	(\x y -> f $ J.pixelAt i (fromIntegral x) (fromIntegral y))

pixelRGBA8ToPixelArgb32 :: J.PixelRGBA8 -> C.PixelArgb32
pixelRGBA8ToPixelArgb32 (J.PixelRGBA8 r g b a) = let u = (a, 0xff) in
	C.PixelArgb32 a (r `unit` u) (g `unit` u) (b `unit` u)

juicyRGBA8ToCairoArgb32 :: J.Image J.PixelRGBA8 -> C.Argb32
juicyRGBA8ToCairoArgb32 = juicyToCairo pixelRGBA8ToPixelArgb32
