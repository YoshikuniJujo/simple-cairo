{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Text (
	cairoSelectFontFace, cairoSetFontSize, cairoShowText, cairoTextExtents
	) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Graphics.Cairo.Monad
import Graphics.Cairo.Types
import Graphics.Cairo.Values

#include <cairo.h>

foreign import ccall "cairo_select_font_face" c_cairo_select_font_face ::
	Ptr (CairoT s) -> CString ->
	#{type cairo_font_slant_t} -> #{type cairo_font_weight_t} -> IO ()

cairoSelectFontFace :: CairoMonad s m =>
	CairoT s -> T.Text -> CairoFontSlantT -> CairoFontWeightT -> m ()
cairoSelectFontFace cr ff (CairoFontSlantT sl) (CairoFontWeightT wt) =
	(`argCairoT` cr) \pcr -> encode ff \cs ->
		c_cairo_select_font_face pcr cs sl wt

foreign import ccall "cairo_set_font_size" c_cairo_set_font_size ::
	Ptr (CairoT s) -> #{type double} -> IO ()

cairoSetFontSize :: CairoMonad s m => CairoT s -> #{type double} -> m ()
cairoSetFontSize cr fs = (`argCairoT` cr) \pcr -> c_cairo_set_font_size pcr fs

foreign import ccall "cairo_text_extents" c_cairo_text_extents ::
	Ptr (CairoT s) -> CString -> Ptr CairoTextExtentsT -> IO ()

cairoTextExtents :: CairoMonad s m => CairoT s -> T.Text -> m CairoTextExtentsT
cairoTextExtents cr t = (`argCairoT` cr) \pcr ->
	encode t \cs -> alloca \p -> c_cairo_text_extents pcr cs p *> peek p

encode :: T.Text -> (CString -> IO a) -> IO a
encode t = BS.useAsCString $ T.encodeUtf8 t

foreign import ccall "cairo_show_text" c_cairo_show_text ::
	Ptr (CairoT s) -> CString -> IO ()

cairoShowText :: CairoMonad s m => CairoT s -> T.Text -> m ()
cairoShowText cr t = (`argCairoT` cr) \pcr -> encode t \cs -> c_cairo_show_text pcr cs
