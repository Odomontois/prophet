{-# LANGUAGE RecordWildCards #-}

module Strategy where

import Data.Foldable
import Data.Monoid
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Function
import Data.Ord
import Control.Monad

class Ord a=>Game a where
  game::a->a->Points
  choices::[a]
  mkChoicer::[b]->a->b

data Points = Points {
  server::Integer,
  client::Integer
} deriving (Show, Eq, Ord)

data Strategy c = Strategy {
  choice     ::c,
  points     ::Points,
  subStrat   ::SubStrategy c,
  clientStrat::[c]
}

type SubStrategy c = c-> Maybe (Strategy c)

consider::Eq b=>(a->b)->a->a->All
consider =  ((All.).). on (==)

equalParts = ((getAll .) .)

instance Eq c => Eq (Strategy c) where
  (==) = equalParts $
    consider (client. points) <>
    consider (server. points) <>
    consider choice

serverStrat Strategy{..} = choice: maybe [] serverStrat (subStrat (head clientStrat) )

clientCompare, serverCompare :: Points -> Points -> Ordering
clientCompare = comparing client <> comparing (Down . server)
serverCompare = comparing server <> comparing (Down . client)

instance Ord c => Ord (Strategy c) where
  compare =  comparing (client . points) <>
             comparing (server . points) <>
             comparing choice

instance Monoid Points where
  mempty = Points 0 0
  (Points c1 s1) `mappend` (Points c2 s2) = Points (c1 + c2) (s1 + s2)

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
