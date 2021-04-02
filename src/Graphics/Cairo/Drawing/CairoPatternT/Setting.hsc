{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoPatternT.Setting where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Primitive
import Data.Word
import Graphics.Cairo.Drawing.CairoPatternT.Basic

#include <cairo.h>

class CairoPatternSetting s where
	cairoPatternSet :: (PrimMonad m, IsCairoPatternT pt) => pt (PrimState m) -> s -> m ()
	cairoPatternGet :: (PrimMonad m, IsCairoPatternT pt) => pt (PrimState m) -> m s

newtype CairoExtendT = CairoExtendT #{type cairo_extend_t}

pattern CairoExtendNone :: CairoExtendT
pattern CairoExtendNone <- CairoExtendT #{const CAIRO_EXTEND_NONE} where
	CairoExtendNone = CairoExtendT #{const CAIRO_EXTEND_NONE}

pattern CairoExtendRepeat :: CairoExtendT
pattern CairoExtendRepeat <- CairoExtendT #{const CAIRO_EXTEND_REPEAT} where
	CairoExtendRepeat = CairoExtendT #{const CAIRO_EXTEND_REPEAT}

pattern CairoExtendReflect :: CairoExtendT
pattern CairoExtendReflect <- CairoExtendT #{const CAIRO_EXTEND_REFLECT} where
	CairoExtendReflect = CairoExtendT #{const CAIRO_EXTEND_REFLECT}

pattern CairoExtendPad :: CairoExtendT
pattern CairoExtendPad <- CairoExtendT #{const CAIRO_EXTEND_PAD} where
	CairoExtendPad = CairoExtendT #{const CAIRO_EXTEND_PAD}

instance CairoPatternSetting CairoExtendT where
	cairoPatternSet = cairoPatternSetExtend
	cairoPatternGet = cairoPatternGetExtend

cairoPatternSetExtend :: (PrimMonad m, IsCairoPatternT pt) => pt (PrimState m) -> CairoExtendT -> m ()
cairoPatternSetExtend (toCairoPatternT -> CairoPatternT fpt) (CairoExtendT ex) =
	unsafeIOToPrim $ withForeignPtr fpt \ppt -> c_cairo_pattern_set_extend ppt ex

foreign import ccall "cairo_pattern_set_extend" c_cairo_pattern_set_extend ::
	Ptr (CairoPatternT s) -> #{type cairo_extend_t} -> IO ()

cairoPatternGetExtend :: (PrimMonad m, IsCairoPatternT pt) => pt (PrimState m) -> m CairoExtendT
cairoPatternGetExtend (toCairoPatternT -> CairoPatternT fpt) =
	unsafeIOToPrim $ CairoExtendT <$> withForeignPtr fpt c_cairo_pattern_get_extend

foreign import ccall "cairo_pattern_get_extend" c_cairo_pattern_get_extend ::
	Ptr (CairoPatternT s) -> IO #{type cairo_extend_t}
