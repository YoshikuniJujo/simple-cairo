{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT.Clip where

import Foreign.Ptr
import Control.Monad.Primitive
import Data.CairoContext

cairoClip, cairoClipPreserve :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoClip = (`withCairoT` c_cairo_clip)
cairoClipPreserve = (`withCairoT` c_cairo_clip_preserve)

foreign import ccall "cairo_clip" c_cairo_clip :: Ptr (CairoT s) -> IO ()

foreign import ccall "cairo_clip_preserve" c_cairo_clip_preserve :: Ptr (CairoT s) -> IO ()
