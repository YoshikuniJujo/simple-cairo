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
import Control.Monad.ST

import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal
import Graphics.Cairo.Surfaces.CairoSurfaceTypeT
import Graphics.Cairo.Exception

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
