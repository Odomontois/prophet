{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Game(
  Game(..),
  MoveT(..),
  Move,
  Strategy(..),
  client, server,
  clientStrat,
  serverStrat
) where

import Data.Foldable
import Data.Monoid
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Function
import Data.Ord
import Control.Monad

import DecisionTree

class (Ord a, Ord (Interest a), Monoid (Interest a))=>Game a where
  type Interest a
  game::a->a->Couple (Interest a)
  choices::[a]
  mkChoicer::[b]->a->b

  strategy::SubStrategy a->a->(Move a, Strategy a)
  strategy subStrat serverChoice = let
      subPoints    = game serverChoice <> maybe mempty mvPoints . subStrat
      clientChoice = maximumBy (clientCompare `on` subPoints) choices
      move = Move (serverChoice, clientChoice) (subPoints clientChoice )
      in  ( move, move :< wrapDT subStrat)

  initialStrategy::StratMap a
  initialStrategy = Map.fromList $ map start choices where
    start = strategy (const Nothing)

  nextStrategy::StratMap a -> StratMap a
  nextStrategy set = Map.fromList $ do
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
    cmp = serverCompare `on` mvPoints
    in maximumBy cmp res

type Couple a = (a, a)

client = snd
server = fst

data MoveT c p = Move {
  choice     ::Couple c ,
  points     ::Couple p
} deriving (Eq, Ord)

type Move c = MoveT c (Interest c)

type Strategy c = DecisionTree c (Move c)
type SubStrategy c = c-> Maybe (Strategy c)
type StratMap c = Map (Move c) (Strategy c)

moves = collect (client.choice)

mvPoints::Comonad cm=> cm (Move a)->Couple (Interest a)
mvPoints = points.extract

serverStrat, clientStrat::Strategy c->[c]
serverStrat  = map (server.choice). moves
clientStrat  = map (client.choice). moves

clientCompare, serverCompare ::Ord a => Couple a-> Couple a-> Ordering
clientCompare = comparing client <> comparing (Down . server)
serverCompare = comparing server <> comparing (Down . client)
