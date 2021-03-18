{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.CairoContext where

import Foreign.ForeignPtr

newtype CairoT s = CairoT (ForeignPtr (CairoT s)) deriving Show
