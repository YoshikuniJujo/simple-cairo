{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Title

import Data.List.Length
import Data.CairoImage.Internal

bits :: LengthL 32 Bit
bits = (repeatL O :: LengthL 8 Bit) ++. (repeatL I :: LengthL 8 Bit) ++.
	(repeatL O :: LengthL 8 Bit) ++. (repeatL I :: LengthL 8 Bit)

main :: IO ()
main = do
	putStrLn $ mkTitle "test word 32"
	print (0xff00ff00 :: Integer)
	print $ fromWord32 0xff00ff00
	print $ toWord32 bits
