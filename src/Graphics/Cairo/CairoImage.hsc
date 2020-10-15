{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.CairoImage where

-- import Foreign.Ptr
import Data.Vector
import Data.Word

newtype ImageArgb32 = ImageArgb32 (Vector Word32) deriving Show

-- loadImageArgb32 :: Ptr #{type unsigned char} -> IO ImageArgb32
-- loadImageArgb32 p = 
