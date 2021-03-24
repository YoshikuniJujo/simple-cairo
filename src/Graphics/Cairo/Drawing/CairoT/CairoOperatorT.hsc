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
