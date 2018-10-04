{-|
Problem: <https://adventofcode.com/2017/day/10>

Solution:

General - Nothing special here. Just very careful reading
of the problem statement and the requirements that come
with it.

Part1 - Implement the knot.

Part2 - Implement the sparse/dense hash.
-}
module Day10 where

import Data.Text (split, pack)
import Data.Text.Read (decimal)
import Data.Either (rights)

import Util

type Length = Int
type Hash = [Int]
type Position = Int
type Size = Int

-- | read the input
input :: [Length]
input = map fst $ rights $ map decimal $ split ((==) ',') $ pack $ head $ inputRaw "input/Day10input.txt"

-- | reverse the segment defined by start and length
reverseSegment :: Hash -> Position -> Length -> Hash
reverseSegment hash start length' = shiftRight start $ reverse' length' $ shiftLeft start hash where
  shiftLeft 0 hash' = hash'
  shiftLeft 1 (head':tail') = tail' ++ [head']
  shiftLeft times hash' = shiftLeft (times - 1) (shiftLeft 1 hash')
  shiftRight times hash' = shiftLeft ((length hash') - times) hash'
  reverse' length'' hash' = (reverse $ take length'' hash') ++ (drop length'' hash')

-- | how to pinch a hash
pinch :: (Hash, Position, Size) -> Length -> (Hash, Position, Size)
pinch (hash, position, skip) length' = (nextHash, nextPosition, nextSkip) where
  nextHash = reverseSegment hash position length'
  nextPosition = mod (position + length' + skip) (length hash)
  nextSkip = skip + 1

-- | knot a hash by pinching it with all lengths
knot :: [Length] -> Hash
knot lengths = getHash $ foldl pinch (seed, 0, 0) lengths where
  seed = [0..255]
  getHash (hash, _, _) = hash
