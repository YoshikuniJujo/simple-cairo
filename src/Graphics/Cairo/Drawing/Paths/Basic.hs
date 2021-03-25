module Graphics.Cairo.Drawing.Paths.Basic where

import Foreign.Ptr
import Control.Monad.Primitive
import Data.CairoContext

cairoNewPath :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoNewPath = (`withCairoT` c_cairo_new_path)

foreign import ccall "cairo_new_path" c_cairo_new_path :: Ptr (CairoT s) -> IO ()

cairoNewSubPath :: PrimMonad m => CairoT (PrimState m) -> m ()
cairoNewSubPath = (`withCairoT` c_cairo_new_sub_path)

foreign import ccall "cairo_new_sub_path" c_cairo_new_sub_path :: Ptr (CairoT s) -> IO ()
