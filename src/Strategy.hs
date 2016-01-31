{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleContexts #-}

module Strategy where

import Data.Foldable
import Data.Monoid
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Function
import Data.Ord
import Control.Monad

class (Ord a, Ord (Interest a), Monoid (Interest a))=>Game a where
  type Interest a
  game::a->a->Points (Interest a)
  choices::[a]
  mkChoicer::[b]->a->b

data Points c = Points {
  server::c,
  client::c
} deriving (Eq, Show, Ord)


data Strategy c = Strategy {
  choice     ::c,
  points     ::Points (Interest c),
  subStrat   ::SubStrategy c,
  clientStrat::[c]
}

type SubStrategy c = c-> Maybe (Strategy c)

consider::Eq b=>(a->b)->a->a->All
consider =  ((All.).). on (==)

equalParts = ((getAll .) .)

instance Game c => Eq (Strategy c) where
  (==) = equalParts $
    consider points <>
    consider choice

serverStrat Strategy{..} = choice: maybe [] serverStrat (subStrat (head clientStrat) )

clientCompare, serverCompare ::Ord a => Points a-> Points a-> Ordering
clientCompare = comparing client <> comparing (Down . server)
serverCompare = comparing server <> comparing (Down . client)

instance Game c => Ord (Strategy c) where
  compare =  comparing (client . points) <>
             comparing (server . points) <>
             comparing choice

instance Monoid c=>Monoid (Points c) where
  mempty = Points mempty mempty
  (Points c1 s1) `mappend` (Points c2 s2) = Points (c1 <> c2) (s1 <> s2)

values::(Enum a, Bounded a)=>[a]
values= [minBound..maxBound]

strategy::Game c=>SubStrategy c->c->Strategy c
strategy subStrat choice = let
    subPoints    = game choice <> maybe mempty points . subStrat
    clientChoice = maximumBy (clientCompare `on` subPoints) choices
    history      = maybe [] clientStrat $ subStrat clientChoice
    in Strategy{points = subPoints clientChoice, clientStrat = clientChoice : history, ..}

initial :: Game c=>Set (Strategy c)
initial = Set.fromList $ map start choices where
  start = strategy (const Nothing)

next::Game c=>Set (Strategy c) -> Set (Strategy c)
next set = Set.fromList $ do
  let cs = choices
  subs   <- replicateM (length cs) $ toList set
  choice <- cs
  let sub = Just . mkChoicer subs
  return $ strategy sub choice

serverChoice::Game c=>Int->Strategy c
serverChoice count = let
  ops = replicate (count - 1) next
  run = foldMap Endo ops
  res = appEndo run initial
  cmp = serverCompare `on` points
  in maximumBy cmp res
