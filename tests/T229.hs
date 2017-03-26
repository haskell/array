{-# LANGUAGE CPP #-}

import Data.Array.MArray
import Data.Array.IO
import Data.Word

main :: IO ()
main = do
  -- This should fail due to integer overflow
#if WORD_SIZE == 8
  m <- newArray_ (0,2^62-1) :: IO (IOUArray Int Word32) -- allocates 0 bytes
  readArray m 17 >>= print -- Read some random location in address space
#else
  m <- newArray_ (0,2^30-1) :: IO (IOUArray Int Word32) -- allocates 0 bytes
  readArray m 17 >>= print -- Read some random location in address space
#endif

