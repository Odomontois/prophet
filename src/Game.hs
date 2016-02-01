{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleContexts #-}

module Game(
  Game(..),
  Points(..),
  Strategy(..),
  serverStrat
) where

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

  strategy::SubStrategy a->a->Strategy a
  strategy subStrat choice = let
      subPoints    = game choice <> maybe mempty points . subStrat
      clientChoice = maximumBy (clientCompare `on` subPoints) choices
      history      = maybe [] clientStrat $ subStrat clientChoice
      in Strategy{points = subPoints clientChoice, clientStrat = clientChoice : history, ..}

  initialStrategy::Set (Strategy a)
  initialStrategy = Set.fromList $ map start choices where
    start = strategy (const Nothing)

  nextStrategy::Set (Strategy a) -> Set (Strategy a)
  nextStrategy set = Set.fromList $ do
    let cs = choices
    subs   <- replicateM (length cs) $ toList set
    choice <- cs
    let sub = Just . mkChoicer subs
    return $ strategy sub choice

  serverChoice::Int->Strategy a
  serverChoice count = let
    ops = replicate (count - 1) nextStrategy
    run = foldMap Endo ops
    res = appEndo run initialStrategy
    cmp = serverCompare `on` points
    in maximumBy cmp res

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

serverStrat::Strategy c->[c]
serverStrat Strategy{..} =  choice: maybe [] serverStrat (subStrat (head clientStrat) )

clientCompare, serverCompare ::Ord a => Points a-> Points a-> Ordering
clientCompare = comparing client <> comparing (Down . server)
serverCompare = comparing server <> comparing (Down . client)

instance Game c => Eq (Strategy c) where
  s1 == s2 = points s1 == points s2 && choice s1 == choice s2

instance Game c => Ord (Strategy c) where
  compare =  comparing points <> comparing choice

instance Monoid c=>Monoid (Points c) where
  mempty = Points mempty mempty
  mappend (Points c1 s1) (Points c2 s2) = Points (c1 <> c2) (s1 <> s2)
