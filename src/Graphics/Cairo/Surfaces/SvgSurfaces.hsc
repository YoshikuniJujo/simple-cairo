{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.SvgSurfaces where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C.Types
import Foreign.C.String
import Control.Monad.Primitive
import Data.Word
import Graphics.Cairo.Exception
import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal
import Graphics.Cairo.Surfaces.CairoSurfaceTypeT

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

#include <cairo.h>

newtype CairoSurfaceSvgT s = CairoSurfaceSvgT (ForeignPtr (CairoSurfaceT s)) deriving Show

pattern CairoSurfaceTSvg :: CairoSurfaceSvgT s -> CairoSurfaceT s
pattern CairoSurfaceTSvg sr <- (cairoSurfaceTSvg -> Just sr) where
	CairoSurfaceTSvg = toCairoSurfaceT

cairoSurfaceTSvg :: CairoSurfaceT s -> Maybe (CairoSurfaceSvgT s)
cairoSurfaceTSvg sr@(CairoSurfaceT fsr) = case cairoSurfaceGetType sr of
	CairoSurfaceTypeSvg -> Just $ CairoSurfaceSvgT fsr
	_ -> Nothing

instance IsCairoSurfaceT CairoSurfaceSvgT where
	toCairoSurfaceT (CairoSurfaceSvgT fsr) = CairoSurfaceT fsr

foreign import ccall "cairo_svg_surface_create" c_cairo_svg_surface_create ::
	CString -> CDouble -> CDouble -> IO (Ptr (CairoSurfaceT s))

cairoSvgSurfaceWith :: FilePath -> CDouble -> CDouble -> (CairoSurfaceSvgT RealWorld -> IO a) -> IO ()
cairoSvgSurfaceWith fp w h f = do
	sr@(CairoSurfaceSvgT fsr) <- cairoSvgSurfaceCreate fp w h
	f sr >> withForeignPtr fsr c_cairo_surface_finish
	

cairoSvgSurfaceCreate :: FilePath -> CDouble -> CDouble -> IO (CairoSurfaceSvgT RealWorld)
cairoSvgSurfaceCreate fp w h = withCString fp \cs -> do
	p <- c_cairo_svg_surface_create cs w h
	mkCairoSurfaceSvgT p <* raiseIfErrorPtrSurface p

foreign import ccall "wrapper" c_wrap_cairo_write_func_t ::
	(Ptr a -> CString -> CInt -> IO #{type cairo_status_t}) ->
	IO (FunPtr (Ptr a -> CString -> CInt -> IO #{type cairo_status_t}))

wrapCairoWriteFuncT :: PrimBase m => (Ptr a -> T.Text -> m WriteResult) ->
	IO (FunPtr (Ptr a -> CString -> CInt -> IO #{type cairo_status_t}))
wrapCairoWriteFuncT wf = c_wrap_cairo_write_func_t $ convertCairoWriteFuncTText wf

convertCairoWriteFuncT :: (Ptr a -> String -> IO #{type cairo_status_t}) ->
	Ptr a -> CString -> CInt -> IO #{type cairo_status_t}
convertCairoWriteFuncT wf p cs ln = peekCStringLen (cs, fromIntegral ln) >>= \s -> wf p s

convertCairoWriteFuncTText :: PrimBase m => (Ptr a -> T.Text -> m WriteResult) ->
	Ptr a -> CString -> CInt -> IO #{type cairo_status_t}
convertCairoWriteFuncTText wf p cs ln = writeResultToCairoStatusT
	<$> (T.peekCStringLen (cs, fromIntegral ln) >>= \t -> unsafePrimToIO $ wf p t)

foreign import ccall "cairo_svg_surface_create_for_stream" c_cairo_svg_surface_create_for_stream ::
	FunPtr (Ptr a -> CString -> CInt -> IO #{type cairo_status_t}) -> Ptr a -> CDouble -> CDouble -> IO (Ptr (CairoSurfaceT s))

cairoSvgSurfaceWithForStream :: PrimBase m => (Ptr a -> T.Text -> m WriteResult) -> Ptr a -> CDouble -> CDouble ->
	(CairoSurfaceSvgT (PrimState m) -> m a) -> m ()
cairoSvgSurfaceWithForStream wf cl w h f = do
	sr@(CairoSurfaceSvgT fsr) <- cairoSvgSurfaceCreateForStream wf cl w h
	f sr >> unsafeIOToPrim (withForeignPtr fsr c_cairo_surface_finish)


cairoSvgSurfaceCreateForStream :: PrimBase m => (Ptr a -> T.Text -> m WriteResult) -> Ptr a -> CDouble -> CDouble -> m (CairoSurfaceSvgT (PrimState m))
cairoSvgSurfaceCreateForStream wf cl w h = unsafeIOToPrim do
	p <- (wrapCairoWriteFuncT wf >>= \pwf ->
		c_cairo_svg_surface_create_for_stream pwf cl w h)
	sr <- mkCairoSurfaceSvgT p
	sr <$ raiseIfErrorPtrSurface p

data WriteResult = WriteFailure | WriteSuccess deriving Show

writeResultToCairoStatusT :: WriteResult -> #{type cairo_status_t}
writeResultToCairoStatusT = \case
	WriteFailure -> #{const CAIRO_STATUS_WRITE_ERROR}
	WriteSuccess -> #{const CAIRO_STATUS_SUCCESS}

mkCairoSurfaceSvgT :: Ptr (CairoSurfaceT s) -> IO (CairoSurfaceSvgT s)
mkCairoSurfaceSvgT p = CairoSurfaceSvgT <$> newForeignPtr p (c_cairo_surface_destroy p)
