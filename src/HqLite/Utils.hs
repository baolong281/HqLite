module HqLite.Utils where

import qualified Data.Vector as V

-- searching stuff
-- binary search for index containing item or index that should contain item
binarySearch :: (Ord a) => V.Vector a -> a -> Int
binarySearch vec key =
    let len = V.length vec
     in go 0 (len - 1)
  where
    go low high
        -- when search fails return the low index, which is where the key should be inserted
        | low > high = low
        | otherwise =
            let mid = (low + high) `div` 2
                midVal = vec V.! mid
             in case compare key midVal of
                    LT -> go low (mid - 1)
                    GT -> go (mid + 1) high
                    EQ -> mid
