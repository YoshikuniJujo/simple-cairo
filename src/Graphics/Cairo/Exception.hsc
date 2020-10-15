{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Exception where

import Foreign.Ptr
import Control.Monad.Primitive
import Control.Exception
import Control.Exception.Hierarchy
import Data.Word
import Graphics.Cairo.Monad
import Graphics.Cairo.Types

#include <cairo.h>

data CairoStatusNoMemory = CairoStatusNoMemory deriving Show
data CairoStatusInvalidRestore = CairoStatusInvalidRestore deriving Show
data CairoStatusInvalidPopGroup = CairoStatusInvalidPopGroup deriving Show
data CairoStatusNoCurrentPoint = CairoStatusNoCurrentPoint deriving Show
data CairoStatusOthers = CairoStatusOthers #{type cairo_status_t} deriving Show

exceptionHierarchy Nothing $ ExNode "CairoStatus" [
	ExType ''CairoStatusNoMemory,
	ExType ''CairoStatusInvalidRestore,
	ExType ''CairoStatusInvalidPopGroup,
	ExType ''CairoStatusNoCurrentPoint,
	ExType ''CairoStatusOthers
	]

newtype CairoStatusT = CairoStatusT #{type cairo_status_t} deriving (Show, Eq)

foreign import ccall "cairo_status" c_cairo_status :: Ptr (CairoT s) -> IO #type cairo_status_t

raiseIfError :: PrimMonad m => CairoT (PrimState m) -> m ()
raiseIfError cr = (`argCairoT` cr) \pcr -> do
	st <- c_cairo_status pcr
	case st of
		#{const CAIRO_STATUS_SUCCESS} -> pure ()
		#{const CAIRO_STATUS_NO_MEMORY} -> throw CairoStatusNoMemory
		#{const CAIRO_STATUS_INVALID_RESTORE} -> throw CairoStatusInvalidRestore
		#{const CAIRO_STATUS_INVALID_POP_GROUP} -> throw CairoStatusInvalidPopGroup
		#{const CAIRO_STATUS_NO_CURRENT_POINT} -> throw CairoStatusNoCurrentPoint
		_ -> throw $ CairoStatusOthers st

#enum CairoStatusT, CairoStatusT, CAIRO_STATUS_SUCCESS, \
	CAIRO_STATUS_NO_MEMORY, CAIRO_STATUS_INVALID_RESTORE, \
	CAIRO_STATUS_INVALID_POP_GROUP, CAIRO_STATUS_NO_CURRENT_POINT

{-
pattern CairoStatusSuccess :: CairoStatusT
pattern CairoStatusSuccess = CairoStatusT #const CAIRO_STATUS_SUCCESS

pattern CairoStatusNoMemory :: CairoStatusT
pattern CairoStatusNoMemory = CairoStatusT #const CAIRO_STATUS_NO_MEMORY

pattern CairoStatusInvalidRestore :: CairoStatusT
pattern CairoStatusInvalidRestore = CairoStatusT #const CAIRO_STATUS_INVALID_RESTORE

pattern CairoStatusInvalidPopGroup :: CairoStatusT
pattern CairoStatusInvalidPopGroup = CairoStatusT #const CAIRO_STATUS_INVALID_POP_GROUP

pattern CairoStatusNoCurrentPoint :: CairoStatusT
pattern CairoStatusNoCurrentPoint = CairoStatusT #const CAIRO_STATUS_NO_CURRENT_POINT
	-}
