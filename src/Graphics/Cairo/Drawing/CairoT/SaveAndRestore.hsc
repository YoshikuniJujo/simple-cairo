{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.CairoT.SaveAndRestore where

import Foreign.Ptr
import Control.Monad.Primitive
import Data.CairoContext

cairoSave :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoSave = (`withCairoT` c_cairo_save)

foreign import ccall "cairo_save" c_cairo_save :: Ptr (CairoT s) -> IO ()

cairoRestore :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoRestore = (`withCairoT` c_cairo_restore)

foreign import ccall "cairo_restore" c_cairo_restore :: Ptr (CairoT s) -> IO ()
