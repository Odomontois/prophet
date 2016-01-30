module Iterate where

import Data.Monoid

data Iterate a b = Iterate a b (a -> Iterate a b) | StopIteration

instance Foldable (Iterate a) where
  foldMap f = go mempty where
    go acc StopIteration = acc
    go acc (Iterate s e next) = go (acc <> f e) $ next s

boundIter::(Bounded a, Enum a , Eq a)=>Iterate a a
boundIter = go minBound where
  go x = Iterate x x next
  next x  | x == maxBound = StopIteration
          | otherwise    = go (succ x)
