{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Exception where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Exception
import Control.Exception.Hierarchy
import Data.Word
import Graphics.Cairo.Types

import Data.CairoContext

#include <cairo.h>

data CairoStatusNoMemory = CairoStatusNoMemory deriving Show
data CairoStatusInvalidRestore = CairoStatusInvalidRestore deriving Show
data CairoStatusInvalidPopGroup = CairoStatusInvalidPopGroup deriving Show
data CairoStatusNoCurrentPoint = CairoStatusNoCurrentPoint deriving Show
data CairoStatusInvalidMatrix = CairoStatusInvalidMatrix deriving Show
data CairoStatusInvalidStatus = CairoStatusInvalidStatus deriving Show
data CairoStatusNullPointer = CairoStatusNullPointer deriving Show
data CairoStatusInvalidString = CairoStatusInvalidString deriving Show
data CairoStatusInvalidPathData = CairoStatusInvalidPathData deriving Show
data CairoStatusReadError = CairoStatusReadError deriving Show
data CairoStatusWriteError = CairoStatusWriteError deriving Show
data CairoStatusFileNotFound = CairoStatusFileNotFound deriving Show
data CairoStatusOthers = CairoStatusOthers #{type cairo_status_t} deriving Show

exceptionHierarchy Nothing $ ExNode "CairoStatus" [
	ExType ''CairoStatusNoMemory,
	ExType ''CairoStatusInvalidRestore,
	ExType ''CairoStatusInvalidPopGroup,
	ExType ''CairoStatusNoCurrentPoint,
	ExType ''CairoStatusInvalidMatrix,
	ExType ''CairoStatusInvalidStatus,
	ExType ''CairoStatusNullPointer,
	ExType ''CairoStatusInvalidString,
	ExType ''CairoStatusInvalidPathData,
	ExType ''CairoStatusReadError,
	ExType ''CairoStatusWriteError,
	ExType ''CairoStatusFileNotFound,
	ExType ''CairoStatusOthers
	]

newtype CairoStatusT = CairoStatusT #{type cairo_status_t} deriving (Show, Eq)

foreign import ccall "cairo_status" c_cairo_status :: Ptr (CairoT s) -> IO #type cairo_status_t

raiseIfError :: CairoT s -> IO ()
raiseIfError (CairoT fcr) = withForeignPtr fcr \pcr -> cairoStatusToThrowError =<< c_cairo_status pcr

raiseIfErrorRegion :: CairoRegionT s -> IO ()
raiseIfErrorRegion (CairoRegionT fr) = withForeignPtr fr \r -> cairoStatusToThrowError =<< c_cairo_region_status r

foreign import ccall "cairo_region_status" c_cairo_region_status ::
	Ptr (CairoRegionT s) -> IO #type cairo_status_t

cairoStatusToThrowError :: #{type cairo_status_t} -> IO ()
cairoStatusToThrowError = \case
	#{const CAIRO_STATUS_SUCCESS} -> pure ()
	#{const CAIRO_STATUS_NO_MEMORY} -> throw CairoStatusNoMemory
	#{const CAIRO_STATUS_INVALID_RESTORE} -> throw CairoStatusInvalidRestore
	#{const CAIRO_STATUS_INVALID_POP_GROUP} -> throw CairoStatusInvalidPopGroup
	#{const CAIRO_STATUS_NO_CURRENT_POINT} -> throw CairoStatusNoCurrentPoint
	#{const CAIRO_STATUS_INVALID_MATRIX} -> throw CairoStatusInvalidMatrix
	#{const CAIRO_STATUS_INVALID_STATUS} -> throw CairoStatusInvalidStatus
	#{const CAIRO_STATUS_NULL_POINTER} -> throw CairoStatusNullPointer
	#{const CAIRO_STATUS_INVALID_STRING} -> throw CairoStatusInvalidString
	#{const CAIRO_STATUS_INVALID_PATH_DATA} -> throw CairoStatusInvalidPathData
	#{const CAIRO_STATUS_READ_ERROR} -> throw CairoStatusReadError
	#{const CAIRO_STATUS_WRITE_ERROR} -> throw CairoStatusWriteError
	#{const CAIRO_STATUS_FILE_NOT_FOUND} -> throw CairoStatusFileNotFound
	st -> throw $ CairoStatusOthers st

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
