{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT.Setting where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Monad.Primitive
import Data.Word
import Data.CairoContext
import Graphics.Cairo.Exception

#include <cairo.h>

class CairoSetting s where
	cairoSet :: PrimMonad m => CairoT (PrimState m) -> s -> m ()
	cairoGet :: PrimMonad m => CairoT (PrimState m) -> m s

newtype LineWidth = LineWidth { getLineWidth :: CDouble } deriving Show

instance CairoSetting LineWidth where
	cairoSet cr = cairoSetLineWidth cr . getLineWidth
	cairoGet cr = LineWidth <$> withCairoT cr c_cairo_get_line_width

cairoSetLineWidth :: PrimMonad m => CairoT (PrimState m) -> CDouble -> m ()
cairoSetLineWidth cr w = withCairoT cr \pcr -> c_cairo_set_line_width pcr w

foreign import ccall "cairo_set_line_width" c_cairo_set_line_width ::
	Ptr (CairoT s) -> CDouble -> IO ()

foreign import ccall "cairo_get_line_width" c_cairo_get_line_width ::
	Ptr (CairoT s) -> IO CDouble

data Dash = Dash [CDouble] CDouble deriving Show

instance CairoSetting Dash where
	cairoSet cr (Dash ds ofs) = cairoSetDash cr ds ofs
	cairoGet cr = uncurry Dash <$> cairoGetDash cr

cairoSetDash :: PrimMonad m => CairoT (PrimState m) -> [CDouble] -> CDouble -> m ()
cairoSetDash cr ds ofs = withCairoT cr \pcr -> withArrayLen ds \lds pds -> do
	c_cairo_set_dash pcr pds (fromIntegral lds) ofs
	raiseIfError cr

foreign import ccall "cairo_set_dash" c_cairo_set_dash ::
	Ptr (CairoT s) -> Ptr CDouble -> CInt -> CDouble -> IO ()

cairoGetDash :: PrimMonad m => CairoT (PrimState m) -> m ([CDouble], CDouble)
cairoGetDash cr = withCairoT cr \pcr -> do
	ln <- fromIntegral <$> c_cairo_get_dash_count pcr
	allocaArray ln \ds -> alloca \ofs -> do
		c_cairo_get_dash pcr ds ofs
		(,) <$> peekArray ln ds <*> peek ofs

foreign import ccall "cairo_get_dash_count" c_cairo_get_dash_count :: Ptr (CairoT s) -> IO CInt

foreign import ccall "cairo_get_dash" c_cairo_get_dash ::
	Ptr (CairoT s) -> Ptr CDouble -> Ptr CDouble -> IO ()

newtype FillRule = FillRule #{type cairo_fill_rule_t} deriving Show

pattern FillRuleWinding :: FillRule
pattern FillRuleWinding <- FillRule #{const CAIRO_FILL_RULE_WINDING} where
	FillRuleWinding = FillRule #{const CAIRO_FILL_RULE_WINDING}

pattern FillRuleEvenOdd :: FillRule
pattern FillRuleEvenOdd <- FillRule #{const CAIRO_FILL_RULE_EVEN_ODD} where
	FillRuleEvenOdd = FillRule #{const CAIRO_FILL_RULE_EVEN_ODD}

instance CairoSetting FillRule where
	cairoSet cr (FillRule fr) = withCairoT cr \pcr -> c_cairo_set_fill_rule pcr fr
	cairoGet cr = FillRule <$> withCairoT cr c_cairo_get_fill_rule

foreign import ccall "cairo_set_fill_rule" c_cairo_set_fill_rule ::
	Ptr (CairoT s) -> #{type cairo_fill_rule_t} -> IO ()

foreign import ccall "cairo_get_fill_rule" c_cairo_get_fill_rule ::
	Ptr (CairoT s) -> IO #{type cairo_fill_rule_t}
