{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.ImageSurfaces.Juicy (
	cairoImageSurfaceCreateForJuicyImageRgba8,
	cairoImageSurfaceGetJuicyImage
	) where

import Control.Monad.Primitive
import Codec.Picture

import Data.JuicyCairo
import Graphics.Cairo.Types

import Data.CairoImage.Internal hiding (Argb32, pixelAt, Image, Pixel)

import Graphics.Cairo.Surfaces.ImageSurfaces

cairoImageSurfaceCreateForJuicyImageRgba8 :: PrimMonad m =>
	Image PixelRGBA8 -> m (CairoSurfaceT (PrimState m))
cairoImageSurfaceCreateForJuicyImageRgba8 =
	cairoImageSurfaceCreateForCairoImage . CairoImageArgb32 . juicyRGBA8ToCairoArgb32

cairoImageSurfaceGetJuicyImage :: PrimMonad m => CairoSurfaceT (PrimState m) -> m DynamicImage
cairoImageSurfaceGetJuicyImage sfc = (<$> cairoImageSurfaceGetCairoImage sfc) \case
	CairoImageArgb32 a -> ImageRGBA8 $ cairoArgb32ToJuicyRGBA8 a
	_ -> error "yet"
