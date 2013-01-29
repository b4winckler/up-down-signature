module Parsers (parseDoubles) where

import Data.List (unfoldr)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lex.Lazy.Double as B

-- | Parse doubles separated by space or separator character (as determined by
-- 'isSeparator').  Anything else is translated into a 'Nothing'.
--
-- Examples:
--
-- >  parseDoubles "1 2.3"   = [Just 1.0,Just 2.3]
-- >  parseDoubles "1,,2.3," = [Just 1.0,Nothing,Just 2.3,Nothing]
-- >  parseDoubles "1 NA 3"  = [Just 1.0,Nothing,Just 3.0]
--
parseDoubles :: B.ByteString -> [Maybe Double]
parseDoubles bs | B.null bs               = []
                | isSeparator (B.last bs) = ds ++ [Nothing]
                | otherwise               = ds
  where
    ds = unfoldr go (skipWhitespace bs)
    go xs | B.null xs = Nothing
          | otherwise = case B.readDouble xs of
                          Just (d, ys) -> Just (Just d, skipSep ys)
                          Nothing      -> Just (Nothing, skipSep $ skipNA xs)

isWhitespace :: Char -> Bool
isWhitespace c = c == ' ' || c == '\t'

isSeparator :: Char -> Bool
isSeparator c  = c `elem` ",;"

skipWhitespace :: B.ByteString -> B.ByteString
skipWhitespace = B.dropWhile isWhitespace

skipNA :: B.ByteString -> B.ByteString
skipNA = B.dropWhile (not . \c -> isWhitespace c || isSeparator c)

skipSep :: B.ByteString -> B.ByteString
skipSep = skipWhitespace . skip . skipWhitespace
  where
    skip bs = case B.uncons bs of
                Just (x, bs') -> if isSeparator x then bs' else B.cons x bs'
                Nothing       -> B.empty
