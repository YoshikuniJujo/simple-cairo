{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.TagsAndLinks where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Control.Monad.Primitive
import Data.CairoContext

import qualified Data.Text as T

#include <cairo.h>

cairoTagLinkUri :: PrimMonad m => CairoT (PrimState m) -> Uri -> m a -> m a
cairoTagLinkUri (CairoT fcr) u m = do
	unsafeIOToPrim $ withForeignPtr fcr \pcr -> do
		tl <- newCString #{const_str CAIRO_TAG_LINK}
		c_cairo_tag_begin pcr tl =<< newCString ("uri='" ++ u ++ "'")
	m <* unsafeIOToPrim (
		withForeignPtr fcr \pcr -> c_cairo_tag_end pcr
			=<< newCString #{const_str CAIRO_TAG_LINK} )

type Uri = String

foreign import ccall "cairo_tag_begin" c_cairo_tag_begin ::
	Ptr (CairoT s) -> CString -> CString -> IO ()

foreign import ccall "cairo_tag_end" c_cairo_tag_end ::
	Ptr (CairoT s) -> CString -> IO ()
