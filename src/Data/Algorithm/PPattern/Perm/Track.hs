{-|
Module      : Data.Algorithm.PPattern.Perm.Track
Description : Random generation of k-track permutations.
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental
-}

module Data.Algorithm.PPattern.Perm.Track
(
  -- * generating
  rand
, randExact
)
  where

    import qualified Data.List     as List
    import qualified Data.Foldable as Foldable
    import qualified System.Random

    import qualified Data.Algorithm.PPattern.Perm         as Perm
    import qualified Data.Algorithm.PPattern.IntPartition as IntPartition
    import qualified Data.Algorithm.PPattern.Random       as Random

    {-|
      'rand' takes two integers 'n' and 'k' and a random generator 'g'.
      It returns a random (non-uniform) permutation of length 'n' that is
      the union of exactly 'k' increasings sequences, together with a new random
      generator.
    -}
    randExact :: System.Random.RandomGen g => Int -> Int -> g -> (Perm.Perm, g)
    randExact n k = aux . rand n k
      where
        aux (p, g)
          | Perm.Monotone.longestDecreasingLength p < k = aux (p', g')
          | otherwise                                   = (p, g)
          where
            (p', g') = rand n k g

    {-|
      'rand' takes two integers 'n' and 'k' and a random generator 'g'.
      It returns a random (non-uniform) permutation of length 'n' that is
      the union of at most 'k' increasings sequences, together with a new random
      generator.
    -}
    rand :: System.Random.RandomGen g => Int -> Int -> g -> (Perm.Perm, g)
    rand n k g
      | k > n     = (Perm.empty, g)
      | otherwise = (p, g''')
      where
        -- rand int partition
        (intPartition, g') = IntPartition.randIntPartition n k g
        partitionAsList = IntPartition.toList intPartition
        (partitionAsIncreasingLists, g'') = mkTracks partitionAsList g'

        -- random shuffle
        (xs, g''') = Random.randShuffle partitionAsIncreasingLists g''

        -- make permutation
        p = Perm.mk xs

    -- 'mkTracks xs' constructs increasing lists, where the length of each
    -- list is given by the elements of 'xs'.
    mkTracks :: System.Random.RandomGen g => [Int] -> g -> ([[Int]], g)
    mkTracks ls = mkTracksAux [] [1..n] ls
      where
        n = Foldable.sum ls

    -- mkTracks auxiliary function.
    mkTracksAux :: System.Random.RandomGen g =>
      [[Int]] -> [Int] -> [Int] -> g -> ([[Int]], g)
    mkTracksAux acc _  []       g = (acc, g)
    mkTracksAux acc xs (l : ls) g = mkTracksAux (ys : acc) (xs List.\\ ys) ls g'
      where
        (ys, g') = Random.sample l xs g
