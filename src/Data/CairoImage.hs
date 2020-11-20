{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.CairoImage (
	-- * Class Image and ImageMut
	Image(..), ImageMut(..),
	-- * Type CairoImage and CairoImageMut
	CairoImage, CairoImageMut,
	-- * Image Format
	-- ** ARGB32
	PixelArgb32(..),
	pattern CairoImageArgb32, Argb32,
	pattern CairoImageMutArgb32, Argb32Mut ) where

import Data.CairoImage.Internal
