{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Utilities.CairoMatrixT (
	Matrix(..), IsCairoMatrixT, CairoMatrixT, CairoMatrixRegularT,
	cairoMatrixGet,
	cairoMatrixNew, cairoMatrixRegularNew, cairoMatrixNewIdentity,
	cairoMatrixNewTranslate,
	cairoMatrixNewScale, cairoMatrixRegularNewScale, cairoMatrixNewRotate,
	cairoMatrixTranslate, cairoMatrixRotate, cairoMatrixInvert,
	cairoMatrixMultiply,
	Distance(..), cairoMatrixTransformDistance,
	Point(..), cairoMatrixTransformPoint ) where

import Graphics.Cairo.Utilities.CairoMatrixT.Internal
