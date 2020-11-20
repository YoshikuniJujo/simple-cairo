{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Word
import Data.CairoImage

main :: IO ()
main = do
	let	pxl = PixelArgb32 0x01 0x02 0x03 0x04
		PixelArgb32 a r g b = pxl
	print pxl
	print (a, r, g, b)

foo :: (Word8, Word8, Word8, Word8)
foo = case PixelArgb32 0x01 0x02 0x03 0x04 of
	PixelArgb32 a r g b -> (a, r, g, b)
