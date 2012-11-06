{-# LANGUAGE BangPatterns #-}
-- |
-- Functions for computing up-down signatures distribution.

module Data.UpDownSignature (
    -- * Signature distribution
    cdf
  , pdf
    -- * Signature score
  , score
  , approxScore
  ) where

import Control.Applicative ((<$>))
import Data.Bits
import Data.List (foldl', group, unfoldr)
import Data.UpDownSignature.Utility (log2, fac, fst3, paths)
import System.Random.MWC
import qualified Data.MemoCombinators as Memo


-- Internal representation of an up-down signature.
--
-- Signatures values are created using 'signature'.  Do not assume that it will
-- always be an Int.
--
-- Permutations on [0..N] have 2^(N-1) signatures, so Int should suffice
-- since we can only handle N of the order 20 anyway.
type Signature = Int

-- The laps of an ordered list is the maximal number of points that are
-- ordered monotonically.  For example:
--
--   laps [1,2.2,3,0,4] = [2,1,1]
type Laps = [Int]

-- Representation of a count of permutations.
--
-- The number of permutations with a given signature grows very quickly, so
-- Integer becomes necessary even for small signatures.
type Count = Integer


-- Largest signature that is likely to be used.  Smaller signatures are subject
-- to memoization.
maxSignature :: Signature
maxSignature = bit 20

-- Convert ordered data to a signature.  A standing assumption is that no two
-- data points have the same order (this is not checked).
signature :: Ord a => [a] -> Signature
signature = lapsToSignature . laps

-- Convert laps to signature.
-- It is assumed that the laps only consists of positive integers!
--
-- Invariant:
--
--   lapsToSignature [n] = 2^n - 1
lapsToSignature :: Laps -> Signature
lapsToSignature l = fst3 $ foldl' f (0,0,odd $ length l) l
  where
    f (a,n,p) k = (if p then a + (2^k - 1) `shiftL` n else a, n+k, not p)

-- Convert ordered data to laps.
laps :: Ord a => [a] -> Laps
laps = map length . group . unfoldr isIncreasing
  where
    isIncreasing (x:y:zs) = Just (x<y, y:zs)
    isIncreasing _        = Nothing

-- Count the number of permutations with the given signature.
-- (This is memoized because it would be too slow otherwise.)
count :: Signature -> Count
count = Memo.arrayRange (1, maxSignature) count'
  where
    count' n | n > 1     = foldl' (\ !a !b -> a + count b) 0 (predecessors n)
             | n == 1    = 2
             | otherwise = 0

-- The predecessors of 'n' are all numbers which can be produced by removing
-- one bit from a block.  If the topmost block consists of exactly one 1, then
-- the result is inverted.
--
-- Examples: (postfix 'b' indicates base-2)
--
--   predecessors 110b = [11b, 10b]
--   predecessors 101b = [10b,11b,10b]  (Note: the last 10b is 01b inverted)
predecessors :: Signature -> [Signature]
predecessors n = go 0 n
  where
    go  _  0 = []
    go !k !r
      -- Special case for topmost block having exactly one digit
      | k' == 1 && r' == 0 = [n .&. mask `xor` mask]
      | otherwise = n .&. mask + n `shiftR` (k+1) `shiftL` k : go (k+k') r'
      where
        mask    = 1 `shiftL` k - 1
        (k',r') = shiftBlock r

-- Shift away block of least significant ones or zeros and return how many
-- digits were shifted as well as the shifted number.
--
-- Examples: (postfix 'b' indicates base-2)
--
--   shiftBlock 10111b = (3,10b)
--   shiftBlock 10b    = (1,1b)
--   shiftBlock 1b     = (1,0)
shiftBlock :: Signature -> (Int, Signature)
shiftBlock r = go 0 (r `mod` 2) r
  where
    go  c  _  0   = (c,0)
    go !c !p !n
      | p == p'   = go (c+1) p n'
      | otherwise = (c,n)
      where
        (n',p') = n `divMod` 2


-- | Probability that a permutation has the given signature.
prob :: Signature -> Double
prob n = count n `bigDiv` fac (log2 n + 2)

-- | Probability that a permutation has a signature that is not more commmon
-- than the given signature.
cumulative :: Signature -> Double
cumulative = Memo.arrayRange (1, maxSignature) cumulative'
  where
    cumulative' n = sum (filter (<= count n) counts) `bigDiv` fac (m+1)
      where
        counts = map count [2^(m-1)..2^m - 1]
        m      = 1 + log2 n

bigDiv :: (Real a, Real b, Fractional c) => a -> b -> c
bigDiv x y = fromRational $ toRational x / toRational y

-- | Calculate score and its standard error for the given paths.
-- The standard error is estimated using the formula for the standard error of
-- the mean.
scorePaths :: (Ord a) => [[a]] -> (Double, Double)
scorePaths = estimate . sampleMeanVarLength . map cdf
  where
    estimate (m, v, n) = (m, sqrt (v / fromIntegral n))

-- | Robustly compute mean and sample variance in one go (variance estimate is
-- unbiased by using denominator n-1).
sampleMeanVarLength :: [Double] -> (Double, Double, Int)
sampleMeanVarLength = mvl . foldl' step (0, 0, 1)
  where
    mvl  (m, q, k)      = let n = k - 1 in (m, q / fromIntegral (n - 1), n)
    step (!m, !q, !k) x = let m' = m + (x - m) / fromIntegral k
                          in (m', q + (x - m) * (x - m'), k + 1)

-- | Pick one element of the given list.
pick :: [a] -> GenIO -> IO a
pick xs gen = (xs !!) <$> uniformR (0, length xs - 1) gen

-- | Pick one path from the given configuration.
pickPath :: [[a]] -> GenIO -> IO [a]
pickPath xs gen = mapM (`pick` gen) xs

-- | Pick one path from each configuration.
pickPaths :: [[[a]]] -> GenIO -> IO [[a]]
pickPaths xs gen = mapM (`pickPath` gen) xs


-- | Probability that a permutation picked at random has the same signature as
-- the given ordered list.
pdf :: (Ord a) => [a] -> Double
pdf = prob . signature

-- | Probability that a permutation picked at random has a signature which is
-- not more common than the signature of the given ordered list.
--
-- We say that n is not more common than m iff @count n <= count m@.
--
-- Another way to think of this is that @cumulative n@ is near 1 if most
-- signatures are more unusual than n and it is near 0 if n is one of the
-- most unusual signatures.
cdf :: (Ord a) => [a] -> Double
cdf = cumulative . signature

-- | Score for a configuration of points.  This equals the geometric mean of
-- the cumulative probabilities of all paths through the configuration.
score :: (Ord a)
      => [[a]]            -- ^ Configuration
      -> (Double, Double) -- ^ Exact score and standard error (which is 0)
score = flip (,) 0 . fst . scorePaths . paths

-- | Approximate the score for a configuration of points by sampling given
-- number of times from all possible paths through the configuration.
--
-- The difference between 'approxScore' and 'score' is that the latter
-- calculates 'cdf' for all paths through the configuration.  This number grows
-- very rapidly with the number of points in the configuration so 'approxScore'
-- may have to be used for configurations that are not small.
approxScore :: (Ord a)
            => GenIO                -- ^ Random number generator
            -> Int                  -- ^ Number of times to sample all paths
            -> [[a]]                -- ^ Configuration
            -> IO (Double, Double)  -- ^ Approximate score and standard error
approxScore gen n conf = scorePaths <$> (pickPaths (replicate n conf) gen)
