{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.TagsAndLinks where

import Foreign.Ptr
import Foreign.C.String
import Data.CairoContext

foreign import ccall "cairo_tag_begin" c_cairo_tag_begin ::
	Ptr (CairoT s) -> CString -> CString -> IO ()

foreign import ccall "cairo_tag_end" c_cairo_tag_end ::
	Ptr (CairoT s) -> CString -> IO ()
