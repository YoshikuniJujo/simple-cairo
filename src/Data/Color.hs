{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Color where

import Data.Word

data Rgb = RgbWord8_ Word8 Word8 Word8 | RgbDouble_ Double Double Double
	deriving Show

{-# COMPLETE RgbWord8 #-}

pattern RgbWord8 :: Word8 -> Word8 -> Word8 -> Rgb
pattern RgbWord8 r g b <- (fromRgbWord8 -> (r, g, b))
	where RgbWord8 = RgbWord8_

fromRgbWord8 :: Rgb -> (Word8, Word8, Word8)
fromRgbWord8 (RgbWord8_ r g b) = (r, g, b)
fromRgbWord8 (RgbDouble_ r g b) = (r', g', b')
	where
	r' = round $ r * 0xff
	g' = round $ g * 0xff
	b' = round $ b * 0xff

{-# COMPLETE RgbDouble #-}

pattern RgbDouble :: Double -> Double -> Double -> Rgb
pattern RgbDouble r g b <- (fromRgbDouble -> (r, g, b))

fromRgbDouble :: Rgb -> (Double, Double, Double)
fromRgbDouble (RgbWord8_ r g b) = (r', g', b')
	where
	r' = fromIntegral r / 0xff
	g' = fromIntegral g / 0xff
	b' = fromIntegral b / 0xff
fromRgbDouble (RgbDouble_ r g b) = (r, g, b)

rgbDouble :: Double -> Double -> Double -> Maybe Rgb
rgbDouble r g b
	| 0 <= r && r <= 1 && 0 <= g && g <= 1 && 0 <= b && b <= 1 =
		Just $ RgbDouble_ r g b
	| otherwise = Nothing
