{-# LANGUAGE LambdaCase, ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Color (
	Rgb, pattern RgbWord8, pattern RgbDouble, rgbDouble,
	Rgba, pattern RgbaWord8 ) where

import Foreign.C.Types
import Data.Word

data Rgb
	= RgbWord8_ Word8 Word8 Word8
	| RgbDouble_ CDouble CDouble CDouble
	deriving Show

{-# COMPLETE RgbWord8 #-}

pattern RgbWord8 :: Word8 -> Word8 -> Word8 -> Rgb
pattern RgbWord8 r g b <- (fromRgbWord8 -> (r, g, b))
	where RgbWord8 = RgbWord8_

fromRgbWord8 :: Rgb -> (Word8, Word8, Word8)
fromRgbWord8 = \case
	RgbWord8_ r g b -> (r, g, b)
	RgbDouble_ r g b ->
		let [r', g', b'] = cDoubleToWord8 <$> [r, g, b] in (r', g', b')

{-# COMPLETE RgbDouble #-}

pattern RgbDouble :: CDouble -> CDouble -> CDouble -> Rgb
pattern RgbDouble r g b <- (fromRgbDouble -> (r, g, b))

fromRgbDouble :: Rgb -> (CDouble, CDouble, CDouble)
fromRgbDouble = \case
	RgbWord8_ r g b ->
		let [r', g', b'] = word8ToCDouble <$> [r, g, b] in (r', g', b')
	RgbDouble_ r g b -> (r, g, b)

rgbDouble :: CDouble -> CDouble -> CDouble -> Maybe Rgb
rgbDouble r g b
	| 0 <= r && r <= 1 && 0 <= g && g <= 1 && 0 <= b && b <= 1 =
		Just $ RgbDouble_ r g b
	| otherwise = Nothing

data Rgba
	= RgbaWord8_ Word8 Word8 Word8 Word8
	| RgbaDouble_ CDouble CDouble CDouble CDouble
	deriving Show

{-# COMPLETE RgbaWord8 #-}

pattern RgbaWord8 :: Word8 -> Word8 -> Word8 -> Word8 -> Rgba
pattern RgbaWord8 r g b a <- (fromRgbaWord8 -> (r, g, b, a))
	where RgbaWord8 = RgbaWord8_

fromRgbaWord8 :: Rgba -> (Word8, Word8, Word8, Word8)
fromRgbaWord8 = \case
	RgbaWord8_ r g b a -> (r, g, b, a)
	RgbaDouble_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] = cDoubleToWord8 <$> [r, g, b, a]

cDoubleToWord8 :: CDouble -> Word8
cDoubleToWord8 = round . (* 0xff)

word8ToCDouble :: Word8 -> CDouble
word8ToCDouble = (/ 0xff) . fromIntegral
