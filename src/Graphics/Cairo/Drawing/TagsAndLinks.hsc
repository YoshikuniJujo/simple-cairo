{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.TagsAndLinks where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Control.Monad.Primitive
import Data.CairoContext

#include <cairo.h>

cairoTagLinkInternal :: PrimMonad m =>
	CairoT (PrimState m) -> Name -> m a -> m a
cairoTagLinkInternal cr d = cairoTagLinkInternalDestPage cr $ Left d

cairoTagLinkInternalDestPage :: PrimMonad m => CairoT (PrimState m) ->
	Either Name (Int, (Double, Double)) -> m a -> m a
cairoTagLinkInternalDestPage (CairoT fcr) d m = do
	unsafeIOToPrim $ withForeignPtr fcr \pcr -> do
		tl <- newCString #{const_str CAIRO_TAG_LINK}
		c_cairo_tag_begin pcr tl =<< internalAttributes d
	m <* unsafeIOToPrim (
		withForeignPtr fcr \pcr -> c_cairo_tag_end pcr
			=<< newCString #{const_str CAIRO_TAG_LINK} )

internalAttributes :: Either Name (Int, (Double, Double)) -> IO CString
internalAttributes = \case
	Left n -> newCString $ "dest='" ++ n ++ "'"
	Right (p, (x, y)) -> newCString $ "page=" ++ show p ++ " pos=[" ++ show x ++ " " ++ show y ++ "]"

cairoTagLinkUri :: PrimMonad m => CairoT (PrimState m) -> Uri -> m a -> m a
cairoTagLinkUri (CairoT fcr) u m = do
	unsafeIOToPrim $ withForeignPtr fcr \pcr -> do
		tl <- newCString #{const_str CAIRO_TAG_LINK}
		c_cairo_tag_begin pcr tl =<< newCString ("uri='" ++ u ++ "'")
	m <* unsafeIOToPrim (
		withForeignPtr fcr \pcr -> c_cairo_tag_end pcr
			=<< newCString #{const_str CAIRO_TAG_LINK} )

type Uri = String

cairoTagDestination :: PrimMonad m => CairoT (PrimState m) -> Name -> m a -> m a
cairoTagDestination (CairoT fcr) n m = do
	unsafeIOToPrim $ withForeignPtr fcr \pcr -> do
		td <- newCString #{const_str CAIRO_TAG_DEST}
		c_cairo_tag_begin pcr td =<< newCString ("name='" ++ n ++ "'")
	m <* unsafeIOToPrim (
		withForeignPtr fcr \pcr -> c_cairo_tag_end pcr
			=<< newCString #{const_str CAIRO_TAG_DEST} )

type Name = String

foreign import ccall "cairo_tag_begin" c_cairo_tag_begin ::
	Ptr (CairoT s) -> CString -> CString -> IO ()

foreign import ccall "cairo_tag_end" c_cairo_tag_end ::
	Ptr (CairoT s) -> CString -> IO ()
