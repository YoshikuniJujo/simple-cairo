{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.CairoSurfaceT.Internal where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Data.Word
import System.IO.Unsafe

import Graphics.Cairo.Surfaces.CairoSurfaceTypeT

import Graphics.Cairo.Template

#include <cairo.h>

class IsCairoSurfaceT sr where
	toCairoSurfaceT :: sr s -> CairoSurfaceT s

instance IsCairoSurfaceT CairoSurfaceT where toCairoSurfaceT = id

newtype CairoSurfaceT s = CairoSurfaceT (ForeignPtr (CairoSurfaceT s)) deriving Show

mkCairoSurfaceT :: Ptr (CairoSurfaceT s) -> IO (CairoSurfaceT s)
mkCairoSurfaceT p = CairoSurfaceT <$> newForeignPtr p (c_cairo_surface_destroy p)

foreign import ccall "cairo_surface_destroy" c_cairo_surface_destroy ::
	Ptr (CairoSurfaceT s) -> IO ()

foreign import ccall "cairo_surface_finish" c_cairo_surface_finish ::
	Ptr (CairoSurfaceT s) -> IO ()

cairoSurfaceGetType :: IsCairoSurfaceT sr => sr s -> CairoSurfaceTypeT
cairoSurfaceGetType (toCairoSurfaceT -> CairoSurfaceT fsr) = unsafePerformIO
	$ CairoSurfaceTypeT <$> withForeignPtr fsr c_cairo_surface_get_type

foreign import ccall "cairo_surface_get_type" c_cairo_surface_get_type ::
	Ptr (CairoSurfaceT s) -> IO #{type cairo_surface_type_t}

cairoSurfaceGetContent :: IsCairoSurfaceT sr => sr s -> CairoContentT
cairoSurfaceGetContent (toCairoSurfaceT -> CairoSurfaceT fsr) = unsafePerformIO
	$ CairoContentT <$> withForeignPtr fsr c_cairo_surface_get_content

foreign import ccall "cairo_surface_get_content" c_cairo_surface_get_content ::
	Ptr (CairoSurfaceT s) -> IO #{type cairo_content_t}

newtype CairoContentT = CairoContentT #{type cairo_content_t} deriving Show

mkMemberGen ''CairoContentT 'CairoContentT "CairoContentColor" #{const CAIRO_CONTENT_COLOR}
mkMemberGen ''CairoContentT 'CairoContentT "CairoContentAlpha" #{const CAIRO_CONTENT_ALPHA}
mkMemberGen ''CairoContentT 'CairoContentT "CairoContentColorAlpha" #{const CAIRO_CONTENT_COLOR_ALPHA}
