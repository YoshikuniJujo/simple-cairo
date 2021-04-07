{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.PdfSurfaces where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C.Types
import Foreign.C.String
import Control.Monad.ST

import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal

newtype CairoSurfacePdfT s ps = CairoSurfacePdfT (ForeignPtr (CairoSurfaceT s ps)) deriving Show

cairoPdfSurfaceCreate :: FilePath -> CDouble -> CDouble -> IO (CairoSurfaceT s RealWorld)
cairoPdfSurfaceCreate fp w h = CairoSurfaceT <$> withCString fp \cstr -> do
	p <- c_cairo_pdf_surface_create cstr w h
	newForeignPtr p $ c_cairo_surface_destroy p

foreign import ccall "cairo_pdf_surface_create" c_cairo_pdf_surface_create ::
	CString -> CDouble -> CDouble -> IO (Ptr (CairoSurfaceT s ps))

unsafeCairoSurfaceFinish :: CairoSurfaceT s RealWorld -> IO ()
unsafeCairoSurfaceFinish (CairoSurfaceT fsr) =
	withForeignPtr fsr c_cairo_surface_finish
