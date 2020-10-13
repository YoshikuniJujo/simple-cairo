{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tips where

-- import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import System.IO.Unsafe

foo :: Int -> ST s Int
foo n = unsafeIOToST $ alloca \p -> do
	poke p n
	peek p

bar :: Int -> Int
bar n = unsafePerformIO $ alloca \p -> do
	poke p n
	peek p
