-- Ever have trouble deciding whether to use a left or right fold?
-- Por que no los dos?
-- Fold left and right at the same time!
-- Use accumulated values from both directions for each step!

module Fold where

import Data.Set (Set)
import Data.Set qualified as Set

--foldl :: (l -> e -> l) -> l -> [e] -> l
--foldl f z []     = z
--foldl f z (x:xs) = foldl f (f z x) xs
--
--foldr :: (e -> r -> r) -> r -> [e] -> r
--foldr f z []     = z
--foldr f z (x:xs) = f x (foldr f z xs)

-- Fold in both directions at once.
foldrl :: (e -> (l, r) -> (l, r)) -> (l, r) -> [e] -> (l, r)
foldrl _ z [] = z
foldrl f (lz, rz) (x:xs) = (lfold, rf)
  where
    (lf, rf) = f x (lz, rfold)
    (lfold, rfold) = foldrl f (lf, rz) xs

-- Fold in both directions at once with the same function.
foldff :: (e -> a -> a) -> a -> [e] -> (a, a)
foldff f z = foldrl (\e (l, r) -> (f e l, f e r)) (z, z)

-- foldl and foldr are just special cases.
foldl :: (l -> e -> l) -> l -> [e] -> l
foldl f z = fst . foldff (flip f) z

foldr :: (e -> r -> r) -> r -> [e] -> r
foldr f z = snd . foldff f z

data Instance
  = First
  | Intermediate
  | Last
  | Only
  deriving (Show)

exampleInput :: [Int]
exampleInput = [2, 3, 2, 1, 1, 0, 4, 3, 2, 0, 3, 1, 2, 3, 1, 0]

exampleFunction :: Int -> (Set Int, ([Instance], Set Int)) -> (Set Int, ([Instance], Set Int))
exampleFunction i (seenLeft, (insts, seenRight))
  = (Set.insert i seenLeft, (inst : insts, Set.insert i seenRight))
  where
    inst = case (Set.member i seenLeft, Set.member i seenRight) of
      (True,  True)  -> Intermediate
      (False, True)  -> First
      (True,  False) -> Last
      (False, False) -> Only

exampleOutput :: [(Instance, Int)]
exampleOutput = zip instances exampleInput
  where
    (_seenLeft, (instances, _seenRight))
      = foldrl exampleFunction (Set.empty, ([], Set.empty)) exampleInput
