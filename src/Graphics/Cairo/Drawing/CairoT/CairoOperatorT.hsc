{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT.CairoOperatorT where

import Data.Word

#include <cairo.h>

newtype Operator = Operator #{type cairo_operator_t} deriving Show

pattern OperatorClear :: Operator
pattern OperatorClear <- Operator #{const CAIRO_OPERATOR_CLEAR} where
	OperatorClear = Operator #{const CAIRO_OPERATOR_CLEAR}

pattern OperatorSource :: Operator
pattern OperatorSource <- Operator #{const CAIRO_OPERATOR_SOURCE} where
	OperatorSource = Operator #{const CAIRO_OPERATOR_SOURCE}

pattern OperatorOver :: Operator
pattern OperatorOver <- Operator #{const CAIRO_OPERATOR_OVER} where
	OperatorOver = Operator #{const CAIRO_OPERATOR_OVER}

pattern OperatorIn :: Operator
pattern OperatorIn <- Operator #{const CAIRO_OPERATOR_IN} where
	OperatorIn = Operator #{const CAIRO_OPERATOR_IN}

pattern OperatorOut :: Operator
pattern OperatorOut <- Operator #{const CAIRO_OPERATOR_OUT} where
	OperatorOut = Operator #{const CAIRO_OPERATOR_OUT}

pattern OperatorAtop :: Operator
pattern OperatorAtop <- Operator #{const CAIRO_OPERATOR_ATOP} where
	OperatorAtop = Operator #{const CAIRO_OPERATOR_ATOP}

pattern OperatorDest :: Operator
pattern OperatorDest <- Operator #{const CAIRO_OPERATOR_DEST} where
	OperatorDest = Operator #{const CAIRO_OPERATOR_DEST}

pattern OperatorDestOver :: Operator
pattern OperatorDestOver <- Operator #{const CAIRO_OPERATOR_DEST_OVER} where
	OperatorDestOver = Operator #{const CAIRO_OPERATOR_DEST_OVER}

pattern OperatorDestIn :: Operator
pattern OperatorDestIn <- Operator #{const CAIRO_OPERATOR_DEST_IN} where
	OperatorDestIn = Operator #{const CAIRO_OPERATOR_DEST_IN}

pattern OperatorDestOut :: Operator
pattern OperatorDestOut <- Operator #{const CAIRO_OPERATOR_DEST_OUT} where
	OperatorDestOut = Operator #{const CAIRO_OPERATOR_DEST_OUT}

pattern OperatorDestAtop :: Operator
pattern OperatorDestAtop <- Operator #{const CAIRO_OPERATOR_DEST_ATOP} where
	OperatorDestAtop = Operator #{const CAIRO_OPERATOR_DEST_ATOP}

pattern OperatorXor :: Operator
pattern OperatorXor <- Operator #{const CAIRO_OPERATOR_XOR} where
	OperatorXor = Operator #{const CAIRO_OPERATOR_XOR}

pattern OperatorAdd :: Operator
pattern OperatorAdd <- Operator #{const CAIRO_OPERATOR_ADD} where
	OperatorAdd = Operator #{const CAIRO_OPERATOR_ADD}

pattern OperatorSaturate :: Operator
pattern OperatorSaturate <- Operator #{const CAIRO_OPERATOR_ADD} where
	OperatorSaturate = Operator #{const CAIRO_OPERATOR_SATURATE}

pattern OperatorMultiply :: Operator
pattern OperatorMultiply <- Operator #{const CAIRO_OPERATOR_MULTIPLY} where
	OperatorMultiply = Operator #{const CAIRO_OPERATOR_MULTIPLY}

pattern OperatorScreen :: Operator
pattern OperatorScreen <- Operator #{const CAIRO_OPERATOR_SCREEN} where
	OperatorScreen = Operator #{const CAIRO_OPERATOR_SCREEN}

pattern OperatorOverlay :: Operator
pattern OperatorOverlay <- Operator #{const CAIRO_OPERATOR_OVERLAY} where
	OperatorOverlay = Operator #{const CAIRO_OPERATOR_OVERLAY}
