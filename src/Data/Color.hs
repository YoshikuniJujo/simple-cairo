{-# LANGUAGE LambdaCase, ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Color (
	Rgb, pattern RgbWord8, pattern RgbWord16, pattern RgbDouble, rgbDouble,
	Rgba, pattern RgbaWord8, pattern RgbaWord16, pattern RgbaDouble,
	rgbaDouble ) where

import Foreign.C.Types
import Data.Bits
import Data.Word

data Rgb
	= RgbWord8_ Word8 Word8 Word8
	| RgbWord16_ Word16 Word16 Word16
	| RgbDouble_ CDouble CDouble CDouble
	deriving Show

{-# COMPLETE RgbWord8 #-}

pattern RgbWord8 :: Word8 -> Word8 -> Word8 -> Rgb
pattern RgbWord8 r g b <- (fromRgbWord8 -> (r, g, b))
	where RgbWord8 = RgbWord8_

fromRgbWord8 :: Rgb -> (Word8, Word8, Word8)
fromRgbWord8 = \case
	RgbWord8_ r g b -> (r, g, b)
	RgbWord16_ r g b -> (
		fromIntegral $ r `shiftR` 8,
		fromIntegral $ g `shiftR` 8,
		fromIntegral $ b `shiftR` 8 )
	RgbDouble_ r g b ->
		let [r', g', b'] = cDoubleToWord8 <$> [r, g, b] in (r', g', b')

{-# COMPLETE RgbWord16 #-}

pattern RgbWord16 :: Word16 -> Word16 -> Word16 -> Rgb
pattern RgbWord16 r g b <- (fromRgbWord16 -> (r, g, b))
	where RgbWord16 = RgbWord16_

fromRgbWord16 :: Rgb -> (Word16, Word16, Word16)
fromRgbWord16 = \case
	RgbWord8_ (fromIntegral -> r) (fromIntegral -> g) (fromIntegral -> b) ->
		(r `shiftL` 8 .|. r, g `shiftL` 8 .|. g, b `shiftL` 8 .|. b)
	RgbWord16_ r g b -> (r, g, b)
	RgbDouble_ r g b ->
		let [r', g', b'] = cDoubleToWord16 <$> [r, g, b] in (r', g', b')

{-# COMPLETE RgbDouble #-}

pattern RgbDouble :: CDouble -> CDouble -> CDouble -> Rgb
pattern RgbDouble r g b <- (fromRgbDouble -> (r, g, b))

fromRgbDouble :: Rgb -> (CDouble, CDouble, CDouble)
fromRgbDouble = \case
	RgbWord8_ r g b ->
		let [r', g', b'] = word8ToCDouble <$> [r, g, b] in (r', g', b')
	RgbWord16_ r g b ->
		let [r', g', b'] = word16ToCDouble <$> [r, g, b] in (r', g', b')
	RgbDouble_ r g b -> (r, g, b)

rgbDouble :: CDouble -> CDouble -> CDouble -> Maybe Rgb
rgbDouble r g b
	| from0to1 r && from0to1 g && from0to1 b = Just $ RgbDouble_ r g b
	| otherwise = Nothing

data Rgba
	= RgbaWord8_ Word8 Word8 Word8 Word8
	| RgbaWord16_ Word16 Word16 Word16 Word16
	| RgbaDouble_ CDouble CDouble CDouble CDouble
	deriving Show

{-# COMPLETE RgbaWord8 #-}

pattern RgbaWord8 :: Word8 -> Word8 -> Word8 -> Word8 -> Rgba
pattern RgbaWord8 r g b a <- (fromRgbaWord8 -> (r, g, b, a))
	where RgbaWord8 = RgbaWord8_

fromRgbaWord8 :: Rgba -> (Word8, Word8, Word8, Word8)
fromRgbaWord8 = \case
	RgbaWord8_ r g b a -> (r, g, b, a)
	RgbaWord16_ r g b a -> (
		fromIntegral $ r `shiftR` 8,
		fromIntegral $ g `shiftR` 8,
		fromIntegral $ b `shiftR` 8,
		fromIntegral $ a `shiftR` 8 )
	RgbaDouble_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] = cDoubleToWord8 <$> [r, g, b, a]

{-# COMPLETE RgbaWord16 #-}

pattern RgbaWord16 :: Word16 -> Word16 -> Word16 -> Word16 -> Rgba
pattern RgbaWord16 r g b a <- (fromRgbaWord16 -> (r, g, b, a))
	where RgbaWord16 = RgbaWord16_

fromRgbaWord16 :: Rgba -> (Word16, Word16, Word16, Word16)
fromRgbaWord16 = \case
	RgbaWord8_
		(fromIntegral -> r) (fromIntegral -> g)
		(fromIntegral -> b) (fromIntegral -> a) -> (
			r `shiftL` 8 .|. r, g `shiftL` 8 .|. g,
			b `shiftL` 8 .|. b, a `shiftL` 8 .|. a)
	RgbaWord16_ r g b a -> (r, g, b, a)
	RgbaDouble_ r g b a ->
		let [r', g', b', a'] = cDoubleToWord16 <$> [r, g, b, a] in (r', g', b', a')

{-# COMPLETE RgbaDouble #-}

pattern RgbaDouble :: CDouble -> CDouble -> CDouble -> CDouble -> Rgba
pattern RgbaDouble r g b a <- (fromRgbaDouble -> (r, g, b, a))

fromRgbaDouble :: Rgba -> (CDouble, CDouble, CDouble, CDouble)
fromRgbaDouble = \case
	RgbaWord8_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] = word8ToCDouble <$> [r, g, b, a]
	RgbaWord16_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] = word16ToCDouble <$> [r, g, b, a]
	RgbaDouble_ r g b a -> (r, g, b, a)

rgbaDouble :: CDouble -> CDouble -> CDouble -> CDouble -> Maybe Rgba
rgbaDouble r g b a
	| from0to1 r && from0to1 g && from0to1 b && from0to1 a =
		Just $ RgbaDouble_ r g b a
	| otherwise = Nothing

cDoubleToWord8 :: CDouble -> Word8
cDoubleToWord8 = round . (* 0xff)

cDoubleToWord16 :: CDouble -> Word16
cDoubleToWord16 = round . (* 0xffff)

word8ToCDouble :: Word8 -> CDouble
word8ToCDouble = (/ 0xff) . fromIntegral

word16ToCDouble :: Word16 -> CDouble
word16ToCDouble = (/ 0xffff) . fromIntegral

from0to1 :: CDouble -> Bool
from0to1 n = 0 <= n && n <= 1
