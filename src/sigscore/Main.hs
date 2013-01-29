{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_, when)
import Data.List (nub, sort)
import Data.Maybe (catMaybes)
import System.Console.CmdArgs (cmdArgs, (&=), typ, Data, Typeable,
                               help, def, args, typFile, summary, program)
import System.Exit (exitSuccess)
import System.Random.MWC (withSystemRandom)

import Parsers (parseDoubles)
import Utility (logStr, logStrLn, exitBecause, bin, pad)

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.UpDownSignature as Sig


data Args = Args {
    iter  :: Int
  , fname :: Maybe FilePath
  } deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs = Args {
      iter  = 10000 &= typ "NUM" &=
              help "Max number of iterations (default 10000)"
    , fname = def &= args &= typFile
    } &= summary "sigscore v0.3, (C) 2012 Björn Winckler"
      &= program "sigscore"

main :: IO ()
main = withSystemRandom $ \gen -> do
  as <- cmdArgs $ defaultArgs

  -- Read lines from file or stdin (if no file is specified)
  ls <- B.lines <$> case fname as of
                      Just fn -> B.readFile fn
                      _       -> B.getContents

  -- Sanity check 'iter' argument
  let maxIter = if iter as > 0 then iter as else iter defaultArgs

  when (null ls) exitSuccess   -- Nothing to do!

  -- The first line is a list of integers representing categories
  let cats  = map truncate $ catMaybes $ parseDoubles $ head ls
      ncats = length cats

  -- Sanity check and log info about categories
  logStrLn $ foundCategories cats
  when (ncats < 3) $ exitBecause tooFewCategories
  when (length (nub cats) > 20) $ logStrLn tooManyCategories

  -- Each line is a list of doubles, with as many elements as there were in the
  -- categories line.  Missing values and NAs are allowed.  Go through each and
  -- compute the signature score.
  forM_ (zip [(1::Int)..] $ tail ls) $ \(lnum,line) -> do
    let elems  = parseDoubles line
        bins   = map catMaybes $ bin cats elems
    if ncats == length elems && null (filter null bins)
      then
        -- Use approximate score if there are too many points through data
        if countPaths bins > maxIter
          then Sig.approxScore gen maxIter bins >>= putStrLn . showPair
          else putStrLn $ showPair $ Sig.score bins
      else
        putStrLn "NA NA"
    when (lnum `mod` 100 == 0)  $ logStr $ "  " ++ pad 4 (show lnum)
    when (lnum `mod` 1000 == 0) $ logStrLn ""
  logStrLn ""


showPair :: (Show a, Show b) => (a, b) -> String
showPair (x,y) = show x ++ " " ++ show y

countPaths :: [[Double]] -> Int
countPaths = product . map length

tooManyCategories :: String
tooManyCategories = "\
\WARNING: More than 20 categories.  Break up the data into fewer categories\n\
\         to cut down on processing times."

tooFewCategories :: String
tooFewCategories = "\
\ERROR: First line must be a list of at least three integer categories."

foundCategories :: [Int] -> String
foundCategories xs = "Found " ++ show n ++ " "
  ++ if n == 1 then "category" else "categories" ++ ": " ++ show (sort cs)
  where
    cs = nub xs
    n  = length cs
