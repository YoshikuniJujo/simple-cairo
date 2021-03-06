{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Utilities.CairoMatrixT.Internal (
	Matrix(..), IsCairoMatrixT(..),
	CairoMatrixT(..), CairoMatrixRegularT, cairoMatrixCopyFromRegular,
	cairoMatrixAlloc, cairoMatrixGet,
	cairoMatrixNew, cairoMatrixRegularNew,
	cairoMatrixNewIdentity,
	cairoMatrixNewTranslate,
	cairoMatrixNewScale, cairoMatrixRegularNewScale, cairoMatrixNewRotate,
	cairoMatrixTranslate, cairoMatrixScale, cairoMatrixRotate, cairoMatrixInvert,
	cairoMatrixMultiply,
	Distance(..), cairoMatrixTransformDistance,
	Point(..), cairoMatrixTransformPoint ) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Monad
import Control.Monad.Primitive
import Data.Word

import Graphics.Cairo.Exception

#include <cairo.h>

class IsCairoMatrixT mtx where
	toCairoMatrixT :: mtx s -> CairoMatrixT s
	fromCairoMatrixT :: CairoMatrixT s -> mtx s

withCairoMatrixT :: (PrimMonad m, IsCairoMatrixT mtx) =>
	mtx (PrimState m) -> (Ptr (CairoMatrixT (PrimState m)) -> IO a) -> m a
withCairoMatrixT (toCairoMatrixT -> CairoMatrixT fmtx) = unsafeIOToPrim . withForeignPtr fmtx

newtype CairoMatrixT s = CairoMatrixT (ForeignPtr (CairoMatrixT s)) deriving Show

instance IsCairoMatrixT CairoMatrixT where
	toCairoMatrixT = id
	fromCairoMatrixT = id

newtype CairoMatrixRegularT s = CairoMatrixRegularT (ForeignPtr (CairoMatrixT s)) deriving Show

instance IsCairoMatrixT CairoMatrixRegularT where
	toCairoMatrixT (CairoMatrixRegularT f) = CairoMatrixT f
	fromCairoMatrixT (CairoMatrixT f) = CairoMatrixRegularT f

cairoMatrixAlloc :: PrimMonad m =>
	(Ptr (CairoMatrixT (PrimState m)) -> IO a) ->
	m (ForeignPtr (CairoMatrixT (PrimState m)))
cairoMatrixAlloc f = unsafeIOToPrim do
	p <- mallocBytes #{size cairo_matrix_t}
	(($) <$> newForeignPtr <*> free $ p) <* f p

data Matrix = Matrix CDouble CDouble CDouble CDouble CDouble CDouble deriving Show

cairoMatrixGet :: (PrimMonad m, IsCairoMatrixT mtx) => mtx (PrimState m) -> m Matrix
cairoMatrixGet mtx = withCairoMatrixT mtx \p -> Matrix
	<$> #{peek cairo_matrix_t, xx} p <*> #{peek cairo_matrix_t, yx} p
	<*> #{peek cairo_matrix_t, xy} p <*> #{peek cairo_matrix_t, yy} p
	<*> #{peek cairo_matrix_t, x0} p <*> #{peek cairo_matrix_t, y0} p

cairoMatrixNew :: PrimMonad m =>
	CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> 
	m (CairoMatrixT (PrimState m))
cairoMatrixNew xx yx xy yy x0 y0 = CairoMatrixT
	<$> cairoMatrixAlloc \p -> c_cairo_matrix_init p xx yx xy yy x0 y0

cairoMatrixRegularNew :: PrimMonad m =>
	CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> 
	m (Either (CairoMatrixT (PrimState m)) (CairoMatrixRegularT (PrimState m)))
cairoMatrixRegularNew xx yx xy yy x0 y0 = mk
	<$> cairoMatrixAlloc \p -> c_cairo_matrix_init p xx yx xy yy x0 y0
	where mk = case xx * yy - yx * xy of
		0 -> Left . CairoMatrixT; _ -> Right . CairoMatrixRegularT

foreign import ccall "cairo_matrix_init" c_cairo_matrix_init ::
	Ptr (CairoMatrixT s) -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

cairoMatrixNewIdentity :: (PrimMonad m, IsCairoMatrixT mtx) => m (mtx (PrimState m))
cairoMatrixNewIdentity = fromCairoMatrixT . CairoMatrixT
	<$> cairoMatrixAlloc c_cairo_matrix_init_identity

foreign import ccall "cairo_matrix_init_identity" c_cairo_matrix_init_identity ::
	Ptr (CairoMatrixT s) -> IO ()

cairoMatrixNewTranslate :: (PrimMonad m, IsCairoMatrixT mtx) =>
	CDouble -> CDouble -> m (mtx (PrimState m))
cairoMatrixNewTranslate tx ty = fromCairoMatrixT . CairoMatrixT
	<$> cairoMatrixAlloc \p -> c_cairo_matrix_init_translate p tx ty

foreign import ccall "cairo_matrix_init_translate" c_cairo_matrix_init_translate ::
	Ptr (CairoMatrixT s) -> CDouble -> CDouble -> IO ()

cairoMatrixNewScale :: PrimMonad m =>
	CDouble -> CDouble -> m (CairoMatrixT (PrimState m))
cairoMatrixNewScale sx sy = CairoMatrixT
	<$> cairoMatrixAlloc \p -> c_cairo_matrix_init_scale p sx sy

cairoMatrixRegularNewScale :: PrimMonad m =>
	CDouble -> CDouble ->
	m (Either (CairoMatrixT (PrimState m)) (CairoMatrixRegularT (PrimState m)))
cairoMatrixRegularNewScale sx sy = mk
	<$> cairoMatrixAlloc \p -> c_cairo_matrix_init_scale p sx sy
	where mk = case (sx, sy) of
		(_, 0) -> Left . CairoMatrixT
		(0, _) -> Left . CairoMatrixT
		_ -> Right . CairoMatrixRegularT

foreign import ccall "cairo_matrix_init_scale" c_cairo_matrix_init_scale ::
	Ptr (CairoMatrixT s) -> CDouble -> CDouble -> IO ()

cairoMatrixNewRotate :: (PrimMonad m, IsCairoMatrixT mtx) => CDouble -> m (mtx (PrimState m))
cairoMatrixNewRotate rad = fromCairoMatrixT . CairoMatrixT
	<$> cairoMatrixAlloc \p -> c_cairo_matrix_init_rotate p rad

foreign import ccall "cairo_matrix_init_rotate" c_cairo_matrix_init_rotate ::
	Ptr (CairoMatrixT s) -> CDouble -> IO ()

cairoMatrixTranslate :: (PrimMonad m, IsCairoMatrixT mtx) =>
	mtx (PrimState m) -> CDouble -> CDouble -> m ()
cairoMatrixTranslate mtx tx ty =
	withCairoMatrixT mtx \p -> c_cairo_matrix_translate p tx ty

foreign import ccall "cairo_matrix_translate" c_cairo_matrix_translate ::
	Ptr (CairoMatrixT s) -> CDouble -> CDouble -> IO ()

cairoMatrixScale :: PrimMonad m =>
	CairoMatrixT (PrimState m) -> CDouble -> CDouble -> m ()
cairoMatrixScale mtx sx sy =
	withCairoMatrixT mtx \p -> c_cairo_matrix_scale p sx sy

foreign import ccall "cairo_matrix_scale" c_cairo_matrix_scale ::
	Ptr (CairoMatrixT s) -> CDouble -> CDouble -> IO ()

cairoMatrixRotate :: (PrimMonad m, IsCairoMatrixT mtx) =>
	mtx (PrimState m) -> CDouble -> m ()
cairoMatrixRotate mtx rad =
	withCairoMatrixT mtx \p -> c_cairo_matrix_rotate p rad

foreign import ccall "cairo_matrix_rotate" c_cairo_matrix_rotate ::
	Ptr (CairoMatrixT s) -> CDouble -> IO ()

cairoMatrixInvert :: PrimMonad m =>
	CairoMatrixRegularT (PrimState m) -> m ()
cairoMatrixInvert (CairoMatrixRegularT fmtx) =
	unsafeIOToPrim $ withForeignPtr fmtx \pmtx ->
		cairoStatusToThrowError =<< c_cairo_matrix_invert pmtx

foreign import ccall "cairo_matrix_invert" c_cairo_matrix_invert ::
	Ptr (CairoMatrixT s) -> IO #{type cairo_status_t}

cairoMatrixMultiply :: (PrimMonad m, IsCairoMatrixT mtx) =>
	mtx (PrimState m) -> mtx (PrimState m) -> mtx (PrimState m) -> m ()
cairoMatrixMultiply
	(toCairoMatrixT -> CairoMatrixT fr)
	(toCairoMatrixT -> CairoMatrixT fa)
	(toCairoMatrixT -> CairoMatrixT fb) = unsafeIOToPrim
	$ withForeignPtr fr \pr -> withForeignPtr fa \pa -> withForeignPtr fb \pb ->
		c_cairo_matrix_multiply pr pa pb

foreign import ccall "cairo_matrix_multiply" c_cairo_matrix_multiply ::
	Ptr (CairoMatrixT s) -> Ptr (CairoMatrixT s) -> Ptr (CairoMatrixT s) -> IO ()

data Distance = Distance CDouble CDouble deriving Show

cairoMatrixTransformDistance :: (PrimMonad m, IsCairoMatrixT mtx) =>
	mtx (PrimState m) -> Distance -> m Distance
cairoMatrixTransformDistance mtx (Distance dx dy) =
	withCairoMatrixT mtx \pmtx -> alloca \pdx -> alloca \pdy -> do
		zipWithM_ poke [pdx, pdy] [dx, dy]
		c_cairo_matrix_transform_distance pmtx pdx pdy
		Distance <$> peek pdx <*> peek pdy

foreign import ccall "cairo_matrix_transform_distance" c_cairo_matrix_transform_distance ::
	Ptr (CairoMatrixT s) -> Ptr CDouble -> Ptr CDouble -> IO ()

data Point = Point CDouble CDouble deriving Show

cairoMatrixTransformPoint :: (PrimMonad m, IsCairoMatrixT mtx) =>
	mtx (PrimState m) -> Point -> m Point
cairoMatrixTransformPoint mtx (Point x y) =
	withCairoMatrixT mtx \pmtx -> alloca \px -> alloca \py -> do
		zipWithM_ poke [px, py] [x, y]
		c_cairo_matrix_transform_point pmtx px py
		Point <$> peek px <*> peek py

foreign import ccall "cairo_matrix_transform_point" c_cairo_matrix_transform_point ::
	Ptr (CairoMatrixT s) -> Ptr CDouble -> Ptr CDouble -> IO ()

cairoMatrixCopyFromRegular :: PrimMonad m =>
	CairoMatrixRegularT (PrimState m) -> m (CairoMatrixT (PrimState m))
cairoMatrixCopyFromRegular (CairoMatrixRegularT fmtx) = unsafeIOToPrim
	$ CairoMatrixT <$> withForeignPtr fmtx \pmtx -> do
		p <- cairoMatrixCopy pmtx
		newForeignPtr p (free p)

cairoMatrixCopy :: Ptr (CairoMatrixT s) -> IO (Ptr (CairoMatrixT s))
cairoMatrixCopy p0 = do
	p <- mallocBytes #{size cairo_matrix_t}
	#{poke cairo_matrix_t, xx} p =<< (#{peek cairo_matrix_t, xx} p0 :: IO CDouble)
	#{poke cairo_matrix_t, yx} p =<< (#{peek cairo_matrix_t, yx} p0 :: IO CDouble)
	#{poke cairo_matrix_t, xy} p =<< (#{peek cairo_matrix_t, xy} p0 :: IO CDouble)
	#{poke cairo_matrix_t, yy} p =<< (#{peek cairo_matrix_t, yy} p0 :: IO CDouble)
	#{poke cairo_matrix_t, x0} p =<< (#{peek cairo_matrix_t, x0} p0 :: IO CDouble)
	#{poke cairo_matrix_t, y0} p =<< (#{peek cairo_matrix_t, y0} p0 :: IO CDouble)
	pure p
