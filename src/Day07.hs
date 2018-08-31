{-|
Problem: <https://adventofcode.com/2017/day/7>

Solution:

General - This is a tree searching problem.

Part1 - The root is the only node with no parent, means
it is never referenced as a child, means we can find it
by doing the diff between all nodes and all children.

Part2 - Find the root. Go through the tree (top-down) and
find the (bad) node, where the tree unbalanced (for the
first time). Find the wrong weight (the one that is
different to the other ones) and calc you to correct it.
-}
module Day07 where

import Data.List (isInfixOf, (\\))
import Data.List.Split (splitOneOf)
import Data.Maybe (fromJust)

import Data.Tree
import qualified Data.Map as M

import Util (inputRaw)

-- | need that map to build the Tree
type NodeName = String
type NodeStruct = (Int, [NodeName])
type NodeMap = M.Map NodeName NodeStruct

-- | read the input
input :: NodeMap
input = M.fromList $ map parser $ inputRaw "input/Day07input.txt" where
  parser line
    | isNode line = parseNode line
    | otherwise = parseLeaf line where
      isNode line = isInfixOf "->" line
      parseNode line = ((tokens !! 0), ((read $ tokens !! 1), (drop 2 tokens))) where
        -- kozpul (59) -> shavjjt, anujsv, tnzvo
        tokens = filter ((/=) "") $ splitOneOf "(),-> " line
      parseLeaf line = ((tokens !! 0), ((read $ tokens !! 1), [])) where
        -- occxa (60)
        tokens = filter ((/=) "") $ splitOneOf "() " line

-- | find the root element
findRoot :: NodeMap -> NodeName
findRoot allNodes = head $ allNames \\ allChildNames where
  allNames = M.keys allNodes
  allChildNames = concatMap snd $ M.elems allNodes

-- | build a/the tree. Use the node map and a/the root name. Return the root element
build :: NodeMap -> NodeName -> Tree Int
build allNodes root = unfoldTree buildTree root where
  buildTree name = allNodes M.! name

{--
-- | calc the weight of a given element (by suming up the weight of the subtree)
calcWeight :: Element -> Int
calcWeight Root = error "This should not happen"
calcWeight (Leaf _ w _) = w
calcWeight (Node _ w _ cs) = w + (sum $ map calcWeight cs)

-- | check, if the (sub)tree is balanced (if all children have the same weight)
isBalanced :: Element -> Bool
isBalanced Root = error "This should not happen"
isBalanced (Leaf _ _ _) = True
isBalanced (Node _ w _ cs) = all (\c -> (calcWeight c) == checkWeight) cs where
  checkWeight = calcWeight $ head cs

-- | find a/the bad node (by finding the node that is not balanced, but all of its
-- children are balanced; otherwise I have to go down more level)
findBadNode :: Element -> Element
findBadNode Root = error "This should not happen"
findBadNode (Leaf _ _ _) = error "This should not happen"
findBadNode n@(Node _ _ _ cs)
  | not (isBalanced n) && all isBalanced cs = n
  | otherwise = findBadNode $ fromJust $ find (\c -> not (isBalanced c)) cs

-- | for a given (bad) node, return the correct weight
correctWeight :: Element -> Int
correctWeight badNode = (weight bad) - (badWeight - goodWeight) where
  histo = group $ sort $ map calcWeight (children badNode)
  goodWeight = head $ fromJust $ find (\ws -> (length ws) > 1) histo
  badWeight = head $ fromJust $ find (\ws -> (length ws) == 1) histo
  bad = fromJust $ find (\c -> (calcWeight c) == badWeight) (children badNode)
--}
