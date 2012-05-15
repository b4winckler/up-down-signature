module Data.UpDownSignature.Utility (
    choose
  , choosem
  , fac
  , fst3
  , log2
  , logStr
  , logStrLn
  , joinFirstTwo
  , mid
  , pad
  , paths
  , sums
  ) where

import Data.Bits (Bits,shiftR)
import System.IO (hPutStr, hPutStrLn, stderr)


-- | Integer base-2 logarithm
log2 :: (Bits a, Ord a, Num a1) => a -> a1
log2 n | n > 1     = 1 + log2 (n `shiftR` 1)
       | otherwise = 0

-- | Factorial
fac :: Integer -> Integer
fac n = product [1..n]

-- | First element of a triple
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

-- | Binomial coefficient
choose :: Integral a => a -> a -> a
choose _ 0 = 1
choose 0 _ = 0
choose n k = choose (n-1) (k-1) * n `div` k

-- | Multinomial coefficient (choose many / multinomial)
-- choosem :: [Integer] -> Integer
choosem :: Integral a => [a] -> a
choosem ks = product [ n `choose` k | (n,k) <- zip (sums ks) ks ]

-- | Partial sums from first to n-th element
-- sums :: [Integer] -> [Integer]
sums :: Num a => [a] -> [a]
sums = scanl1 (+)

-- | Pick out mid element of a list
mid :: [a] -> a
mid xs = xs !! n
  where
    n = length xs `div` 2

-- | Pad string with spaces so that it is at least 'n' chars wide
pad :: Int -> String -> String
pad n s | n > k     = replicate (n-k) ' ' ++ s
        | otherwise = s
  where
    k = length s

-- | Log string with newline to stderr
logStrLn :: String -> IO ()
logStrLn = hPutStrLn stderr

-- | Log string to stderr
logStr :: String -> IO ()
logStr = hPutStr stderr

-- | Join first two lists with the given function
joinFirstTwo :: ([a] -> [a] -> [a]) -> [[a]] -> [[a]]
joinFirstTwo f (x:y:zs) = f x y : zs
joinFirstTwo _ xs       = xs

-- | Given a list of lists '[l1,...,ln]' enumerate all ways to pick exactly one
-- element from each list 'li'.  For example:
--
-- > paths [[1,2],[],[3,4],[5]] == [[1,3,5],[1,4,5],[2,3,5],[2,4,5]]
--
paths :: [[a]] -> [[a]]
paths [] = [[]]
paths ([]:xss) = paths xss
paths (xs:xss) = [ x:path | x <- xs, path <- paths xss ]

