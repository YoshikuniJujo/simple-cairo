{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.SvgSurfaces where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Control.Monad.ST
import Graphics.Cairo.Exception
import Graphics.Cairo.Surfaces.CairoSurfaceT

foreign import ccall "cairo_svg_surface_create" c_cairo_svg_surface_create ::
	CString -> CDouble -> CDouble -> IO (Ptr (CairoSurfaceT s))

cairoSvgSurfaceCreate :: FilePath -> CDouble -> CDouble -> IO (CairoSurfaceT RealWorld)
cairoSvgSurfaceCreate fp w h = withCString fp \cs -> do
	sr <- makeCairoSurfaceT =<< c_cairo_svg_surface_create cs w h
	sr <$ raiseIfErrorSurface sr
