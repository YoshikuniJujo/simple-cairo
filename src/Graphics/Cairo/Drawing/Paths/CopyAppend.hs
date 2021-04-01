{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Paths.CopyAppend where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Primitive
import Data.CairoContext
import Graphics.Cairo.Drawing.Paths.CairoPathT

cairoCopyPath :: PrimMonad m => CairoT (PrimState m) -> m CairoPathT
cairoCopyPath (CairoT fcr) = unsafeIOToPrim
	$ withForeignPtr fcr \pcr -> mkCairoPathT =<< c_cairo_copy_path pcr

foreign import ccall "cairo_copy_path" c_cairo_copy_path ::
	Ptr (CairoT s) -> IO (Ptr CairoPathT)
