-- |
-- Utility functions.
--
module Utility (
    alternate
  , bin
  , equating
  , exitBecause
  , logStr
  , logStrLn
  , pad
  , pleat
  , trimBins
  ) where

import Data.Ord (comparing)
import Data.List (sort, sortBy, groupBy)
import System.Exit (exitFailure)
import System.IO (hPutStr, hPutStrLn, stderr)


-- | Put data in bins defined by the given categories.
bin :: Ord a => [a] -> [b] -> [[b]]
bin cats = map (snd . unzip)
         . groupBy (equating fst)
         . sortBy (comparing fst)
         . zip cats

-- | Trim bins so that each bin contains at most the given number of elements.
trimBins :: Ord a => Int -> [[a]] -> [[a]]
trimBins n = map (take n . pleat)

-- | Take a list and reorder it so that the median element comes first and the
-- extremal elements come last.  Example:
--
-- >  pleat [1..9] = [5,4,6,3,7,2,8,1]
-- >  pleat [0..9] = [5,4,6,3,7,2,8,1,9,0]
--
-- Note that the input need not be sorted.
pleat :: Ord a => [a] -> [a]
pleat xs = alternate bs (reverse as)
  where
    (as,bs) = splitAt (length xs `div` 2) $ sort xs

-- | Combine two lists into one by alternately taking an element from the
-- first, then from the second.  For example:
--
-- >  alternate [1..3] [5..9] = [1,5,2,6,3,7,8,9]
--
-- Note that no elements from either list are lost.
alternate :: [a] -> [a] -> [a]
alternate (x:xs) (y:ys) = x : y : alternate xs ys
alternate xs      []    = xs
alternate []      ys    = ys

-- | Use e.g. with sorting routines as in
--
-- >  sortBy (equating fst)
--
equating :: Eq a => (t -> a) -> t -> t -> Bool
equating f x y = f x == f y

-- | Exit program with failure and log reason to stderr.
exitBecause :: String -> IO b
exitBecause reason = logStrLn reason >> exitFailure

-- | Log string with newline to stderr
logStrLn :: String -> IO ()
logStrLn = hPutStrLn stderr

-- | Log string to stderr
logStr :: String -> IO ()
logStr = hPutStr stderr

-- | Pad string with spaces so that it is at least 'n' chars wide
pad :: Int -> String -> String
pad n s | n > k     = replicate (n-k) ' ' ++ s
        | otherwise = s
  where
    k = length s
