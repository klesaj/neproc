import Data.List (maximumBy)
import Data.Ord  (comparing)

-- a)
factors :: Int -> [Int]
factors n
  | n < 2     = []
  | otherwise = factor n 2
  where
    factor :: Int -> Int -> [Int]
    factor 1 _ = []
    factor m d
      | d * d > m      = [m]
      | m `mod` d == 0 = d : factor (m `div` d) d
      | otherwise      = factor m (d + 1)


-- b)
gap :: Int -> (Int, Int)
gap n
  | n < 3     = error "Input must be at least 3"
  | otherwise = maximumBy compareGap primePairs
  where
    primesUpTo :: Int -> [Int]
    primesUpTo limit = filter isPrime [2..limit]
        where
            isPrime x = factors x == [x]

    primesList :: [Int]
    primesList = primesUpTo n

    primePairs :: [(Int, Int)]
    primePairs = zip primesList (tail primesList)

    compareGap :: (Int, Int) -> (Int, Int) -> Ordering
    compareGap (p1, q1) (p2, q2) =
      let gap1 = q1 - p1
          gap2 = q2 - p2
      in 
        if gap1 == gap2 
            then compare p2 p1 
        else compare gap1 gap2


