{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.PdfSurfaces where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C.Types
import Foreign.C.String
import Control.Monad.Primitive
import Data.Bits
import Data.Word

import Graphics.Cairo.Exception
import Graphics.Cairo.Surfaces.CairoWriteFuncT
import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal
import Graphics.Cairo.Surfaces.CairoSurfaceTypeT
import Graphics.Cairo.Drawing.TagsAndLinks
import Graphics.Cairo.Template

import qualified Data.ByteString as BS

#include <cairo.h>
#include <cairo-pdf.h>

newtype CairoSurfacePdfT s ps = CairoSurfacePdfT (ForeignPtr (CairoSurfaceT s ps)) deriving Show

pattern CairoSurfaceTPdf :: CairoSurfacePdfT s ps -> CairoSurfaceT s ps
pattern CairoSurfaceTPdf sr <- (cairoSurfaceTPdf -> Just sr) where
	CairoSurfaceTPdf = toCairoSurfaceT

cairoSurfaceTPdf :: CairoSurfaceT s ps -> Maybe (CairoSurfacePdfT s ps)
cairoSurfaceTPdf sr@(CairoSurfaceT fsr) = case cairoSurfaceGetType sr of
	CairoSurfaceTypePdf -> Just $ CairoSurfacePdfT fsr
	_ -> Nothing

instance IsCairoSurfaceT CairoSurfacePdfT where
	toCairoSurfaceT (CairoSurfacePdfT fsr) = CairoSurfaceT fsr

cairoPdfSurfaceWith :: FilePath -> CDouble -> CDouble ->
	(forall s . CairoSurfacePdfT s RealWorld -> IO a) -> IO a
cairoPdfSurfaceWith fp w h f = do
	sr@(CairoSurfacePdfT fsr) <- cairoPdfSurfaceCreate fp w h
	f sr <* withForeignPtr fsr c_cairo_surface_finish

cairoPdfSurfaceCreate :: FilePath -> CDouble -> CDouble -> IO (CairoSurfacePdfT s RealWorld)
cairoPdfSurfaceCreate fp w h = CairoSurfacePdfT <$> withCString fp \cstr -> do
	p <- c_cairo_pdf_surface_create cstr w h
	newForeignPtr p (c_cairo_surface_destroy p) <* raiseIfErrorPtrSurface p

foreign import ccall "cairo_pdf_surface_create" c_cairo_pdf_surface_create ::
	CString -> CDouble -> CDouble -> IO (Ptr (CairoSurfaceT s ps))

unsafeCairoSurfaceFinish :: CairoSurfaceT s RealWorld -> IO ()
unsafeCairoSurfaceFinish (CairoSurfaceT fsr) =
	withForeignPtr fsr c_cairo_surface_finish

cairoPdfSurfaceWithForStream :: PrimBase m =>
	(Ptr a -> BS.ByteString -> m WriteResult) -> Ptr a -> CDouble -> CDouble ->
	(forall s . CairoSurfacePdfT s (PrimState m) -> m a) -> m a
cairoPdfSurfaceWithForStream wf cl w h f = do
	sr@(CairoSurfacePdfT fsr) <- cairoPdfSurfaceCreateForStream wf cl w h
	f sr <* unsafeIOToPrim (withForeignPtr fsr c_cairo_surface_finish)

cairoPdfSurfaceCreateForStream :: PrimBase m =>
	(Ptr a -> BS.ByteString -> m WriteResult) -> Ptr a -> CDouble -> CDouble ->
	m (CairoSurfacePdfT s (PrimState m))
cairoPdfSurfaceCreateForStream wf cl w h = CairoSurfacePdfT <$> unsafeIOToPrim do
	p <- (wrapCairoWriteFuncTByteString wf >>= \pwf ->
		c_cairo_pdf_surface_create_for_stream pwf cl w h)
	newForeignPtr p (c_cairo_surface_destroy p) <* raiseIfErrorPtrSurface p

foreign import ccall "cairo_pdf_surface_create_for_stream"
	c_cairo_pdf_surface_create_for_stream ::
	FunPtr (Ptr a -> CString -> CInt -> IO #{type cairo_status_t}) ->
	Ptr a -> CDouble -> CDouble -> IO (Ptr (CairoSurfaceT s ps))

newtype CairoPdfOutlineT = CairoPdfOutlineT CInt deriving Show

pattern CairoPdfOutlineRoot :: CairoPdfOutlineT
pattern CairoPdfOutlineRoot <- CairoPdfOutlineT #{const CAIRO_PDF_OUTLINE_ROOT} where
	CairoPdfOutlineRoot = CairoPdfOutlineT #{const CAIRO_PDF_OUTLINE_ROOT}

newtype CairoPdfOutlineFlagsT = CairoPdfOutlineFlagsT #{type cairo_pdf_outline_flags_t} deriving Show

mkMemberGen ''CairoPdfOutlineFlagsT 'CairoPdfOutlineFlagsT
	"CairoPdfOutlineFlagOpen" #{const CAIRO_PDF_OUTLINE_FLAG_OPEN}
mkMemberGen ''CairoPdfOutlineFlagsT 'CairoPdfOutlineFlagsT
	"CairoPdfOutlineFlagBold" #{const CAIRO_PDF_OUTLINE_FLAG_BOLD}
mkMemberGen ''CairoPdfOutlineFlagsT 'CairoPdfOutlineFlagsT
	"CairoPdfOutlineFlagItalic" #{const CAIRO_PDF_OUTLINE_FLAG_ITALIC}

join :: CairoPdfOutlineFlagsT -> CairoPdfOutlineFlagsT -> CairoPdfOutlineFlagsT
join (CairoPdfOutlineFlagsT f1) (CairoPdfOutlineFlagsT f2) = CairoPdfOutlineFlagsT $ f1 .|. f2

cairoPdfSurfaceAddOutline :: PrimMonad m =>
	CairoSurfacePdfT s (PrimState m) -> CairoPdfOutlineT -> Name ->
	Either Name (Int, Maybe (Double, Double)) -> [CairoPdfOutlineFlagsT] -> m CairoPdfOutlineT
cairoPdfSurfaceAddOutline (CairoSurfacePdfT fsr) (CairoPdfOutlineT pid) nm d fs = unsafeIOToPrim
	$ CairoPdfOutlineT <$> withForeignPtr fsr \psr -> withCString nm \cnm -> internalAttributes d \cd ->
		c_cairo_pdf_surface_add_outline psr pid cnm cd f
	where
	CairoPdfOutlineFlagsT f = foldr join (CairoPdfOutlineFlagsT 0) fs

foreign import ccall "cairo_pdf_surface_add_outline" c_cairo_pdf_surface_add_outline ::
	Ptr (CairoSurfaceT s ps) -> CInt -> CString -> CString -> #{type cairo_pdf_outline_flags_t} -> IO CInt
