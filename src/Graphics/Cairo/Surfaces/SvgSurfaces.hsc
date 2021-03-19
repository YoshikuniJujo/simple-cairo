{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.SvgSurfaces where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Control.Monad.Primitive
import Data.Word
import Graphics.Cairo.Exception
import Graphics.Cairo.Surfaces.CairoSurfaceT

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

#include <cairo.h>

foreign import ccall "cairo_svg_surface_create" c_cairo_svg_surface_create ::
	CString -> CDouble -> CDouble -> IO (Ptr (CairoSurfaceT s))

cairoSvgSurfaceCreate :: FilePath -> CDouble -> CDouble -> IO (CairoSurfaceT RealWorld)
cairoSvgSurfaceCreate fp w h = withCString fp \cs -> do
	sr <- makeCairoSurfaceT =<< c_cairo_svg_surface_create cs w h
	sr <$ raiseIfErrorSurface sr

foreign import ccall "wrapper" c_wrap_cairo_write_func_t ::
	(Ptr a -> CString -> CInt -> IO #{type cairo_status_t}) ->
	IO (FunPtr (Ptr a -> CString -> CInt -> IO #{type cairo_status_t}))

wrapCairoWriteFuncT :: PrimBase m => (Ptr a -> T.Text -> m ()) ->
	IO (FunPtr (Ptr a -> CString -> CInt -> IO #{type cairo_status_t}))
wrapCairoWriteFuncT wf = c_wrap_cairo_write_func_t $ convertCairoWriteFuncTText wf

convertCairoWriteFuncT :: (Ptr a -> String -> IO #{type cairo_status_t}) ->
	Ptr a -> CString -> CInt -> IO #{type cairo_status_t}
convertCairoWriteFuncT wf p cs ln = peekCStringLen (cs, fromIntegral ln) >>= \s -> wf p s

convertCairoWriteFuncTText :: PrimBase m => (Ptr a -> T.Text -> m ()) ->
	Ptr a -> CString -> CInt -> IO #{type cairo_status_t}
convertCairoWriteFuncTText wf p cs ln = tryCairoWriteFunc $ T.peekCStringLen (cs, fromIntegral ln) >>= \t -> unsafePrimToIO $ wf p t

foreign import ccall "cairo_svg_surface_create_for_stream" c_cairo_svg_surface_create_for_stream ::
	FunPtr (Ptr a -> CString -> CInt -> IO #{type cairo_status_t}) -> Ptr a -> CDouble -> CDouble -> IO (Ptr (CairoSurfaceT s))

cairoSvgSurfaceCreateForStream :: PrimBase m => (Ptr a -> T.Text -> m ()) -> Ptr a -> CDouble -> CDouble -> m (CairoSurfaceT (PrimState m))
cairoSvgSurfaceCreateForStream wf cl w h = unsafeIOToPrim do
	sr <- makeCairoSurfaceT =<< (wrapCairoWriteFuncT wf >>= \pwf ->
		c_cairo_svg_surface_create_for_stream pwf cl w h)
	sr <$ raiseIfErrorSurface sr
