import Data.Array.MArray
import Data.Array.IO
import Data.Word

main :: IO ()
main = do
  -- This should fail due to integer overflow
  m <- newArray_ (0,2^62-1) :: IO (IOUArray Int Word32) -- allocates 0 bytes
  readArray m 17 >>= print -- Read some random location in address space
