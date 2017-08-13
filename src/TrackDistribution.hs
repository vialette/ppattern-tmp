{-|
Module      : TrackDistribution
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Foldable as Foldable
import qualified Data.Monoid   as Monoid
import System.Console.CmdArgs
import System.Random

import qualified Data.Algorithm.PPattern.Perm.Monotone as Perm.Monotone
import qualified Data.Algorithm.PPattern.Perm.Track    as Perm.Track

data Options = Options { size           :: Int
                       , trackParameter :: Int
                       , trials         :: Int
                       , seed           :: Int
                       } deriving (Data, Typeable)

options :: Options
options = Options { size           = def &= help "The permutation size"
                  , trackParameter = def &= help "The track parameter"
                  , trials         = def &= help "The number of trials"
                  , seed           = def &= help "The seed of the random generator"
                  }
                  &= verbosity
                  &= summary "track-distribution v0.1.0.0, (C) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017"
                  &= program "track-distribution"

-- Estimate distribution
distribution :: RandomGen g => Int -> Int -> Int -> g -> [Int]
distribution n k t = aux [] 1
  where
    aux acc i g
      | i > t = acc
      | otherwise = aux (k' : acc) (i+1) g'
      where
        (p, g') = Perm.Track.rand n k g
        k'      = Perm.Monotone.longestDecreasingLength p

go :: RandomGen g => Int -> Int -> Int -> g -> IO ()
go n k t g = Foldable.mapM_ putStr $ fmap f ks
  where
    ks   = distribution n k t g
    f k' = "sample" `Monoid.mappend`
           ","      `Monoid.mappend`
           show n   `Monoid.mappend`
           ","      `Monoid.mappend`
           show k'  `Monoid.mappend`
           "\n"

main :: IO ()
main = do
  opts <- cmdArgs options
  go (size opts) (trackParameter opts) (trials opts) $ mkStdGen (seed opts)
