{-# LANGUAGE ViewPatterns, RecordWildCards #-}

module Strategy where

import Data.Foldable
import Data.Monoid
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Function
import Data.Ord

data Choice = Break | Keep deriving (Show, Eq, Ord, Enum, Bounded)

type Game = Choice -> Choice -> Points

data Points = Points {
  server::Integer,
  client::Integer
} deriving (Show, Eq, Ord)

data Strategy = Strategy {
  choice::Choice,
  points::Points,
  subStrat::Choice->Maybe Strategy,
  clientStrat::[Choice]
}

instance Eq Strategy where
  (==) = ((== EQ) .)  . compare

clientCompare, serverCompare :: Points -> Points -> Ordering
clientCompare = comparing client <> comparing (Down . server)
serverCompare = comparing server <> comparing (Down . client)

instance Ord Strategy where
  compare =  comparing (client . points) <>
             comparing (server . points) <>
             comparing choice

instance Monoid Points where
  mempty = Points 0 0
  (Points c1 s1) `mappend` (Points c2 s2) = Points (c1 + c2) (s1 + s2)

strategy::(Choice -> Maybe Strategy)->Choice->Game->Strategy
strategy subStrat choice game = let
    subPoints  chc = game choice chc  <> maybe mempty points (subStrat chc)
    (clientChoice, maybe [] clientStrat -> history, pts) =
      case clientCompare (subPoints Keep) (subPoints Break) of
        GT -> (Keep , subStrat Keep, subPoints Keep)
        _  -> (Break, subStrat Break,subPoints Break)
    in Strategy{points = pts, clientStrat = clientChoice : history, ..}

initial :: Game -> Set Strategy
initial game = Set.fromList [start Keep, start Break] where
  start choice = strategy (const Nothing) choice game

next :: Game -> Set Strategy -> Set Strategy
next game set = Set.fromList $ do
  keep   <- toList set
  break  <- toList set
  let sub Keep  = keep
      sub Break = break
  choice <- [Break, Keep]
  return $ strategy (Just. sub) choice game

serverChoice :: Game -> Int -> Strategy
serverChoice game count = let
  ops = replicate (count - 1) (next game)
  run = foldMap Endo ops
  res = appEndo run (initial game)
  cmp = serverCompare `on` points
  in maximumBy cmp res
