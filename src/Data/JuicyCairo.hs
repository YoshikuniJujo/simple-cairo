{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.JuicyCairo where

import Data.Maybe

import qualified Data.CairoImage.Internal as C
import qualified Codec.Picture as J

cairoToJuicy :: (C.Image i, J.Pixel p) => (C.Pixel i -> p) -> i -> J.Image p
cairoToJuicy f i = J.generateImage
	(\x y -> f . fromJust $ C.pixelAt i (fromIntegral x) (fromIntegral y))
	(fromIntegral . fst $ C.imageSize i)
	(fromIntegral . snd $ C.imageSize i)

pixelArgb32ToPixelRGBA8 :: C.PixelArgb32 -> J.PixelRGBA8
pixelArgb32ToPixelRGBA8 (C.PixelArgb32 a r g b) = J.PixelRGBA8 r' g' b' a
	where
	r' = r * 0xff `div'` a
	g' = g * 0xff `div'` a
	b' = b * 0xff `div'` a

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
