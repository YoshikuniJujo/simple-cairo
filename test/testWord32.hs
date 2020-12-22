{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Title

import Data.CairoImage.Internal

main :: IO ()
main = do
	putStrLn $ mkTitle "test word 32"
	print $ fromWord32 0xff00ff00
