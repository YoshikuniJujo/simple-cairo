{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT.Clip where

import Foreign.Ptr
import Control.Monad.Primitive
import Data.CairoContext

cairoClip :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoClip = (`withCairoT` c_cairo_clip)

foreign import ccall "cairo_clip" c_cairo_clip :: Ptr (CairoT s) -> IO ()
