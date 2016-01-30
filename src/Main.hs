
{-# LANGUAGE ViewPatterns, RecordWildCards #-}

module Main where

import System.Environment
import Data.Foldable
import Data.Monoid
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Function
import Data.Ord
import Data.Tree
import Data.List
import Data.Maybe

data Choice = Break | Keep deriving (Show, Eq, Ord, Enum, Bounded)

type Game = Choice -> Choice -> Points

data Points = Points {
  server::Integer,
  client::Integer
} deriving (Show, Eq, Ord)

data Strategy = Strategy {
  choice::Choice,
  points::Points,
  keepStrat::Maybe Strategy,
  breakStrat::Maybe Strategy,
  clientStrat::[Choice]
} deriving Show

with = (.) . (.)

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

strategy::Maybe Strategy->Maybe Strategy->Choice->Game->Strategy
strategy
  keepStrat
  breakStrat
  choice
  game = let
    keepPoints   = game choice Keep  <> maybe mempty points keepStrat
    breakPoints  = game choice Break <> maybe mempty points breakStrat
    (cliCh, maybe [] clientStrat -> history, pts) =
      case clientCompare keepPoints breakPoints of
        GT -> (Keep,  keepStrat,  keepPoints)
        _  -> (Break, breakStrat, breakPoints)
    in Strategy{points = pts, clientStrat = cliCh : history, ..}


initial :: Game -> Set Strategy
initial game = Set.fromList [start Keep, start Break] where
  start = flip (strategy Nothing Nothing) game

levels::(Enum a, Bounded a)=>[a]
levels = [minBound..maxBound]

next :: Game -> Set Strategy -> Set Strategy
next game set = Set.fromList $ do
  keep <- toList set
  break <- toList set
  choice <- [minBound..maxBound]
  return $ strategy (Just keep) (Just break) choice game

serverChoice :: Game -> Int -> Strategy
serverChoice game count = let
  ini = initial game
  nxt = next game
  ops = replicate (count - 1) $ Endo nxt
  run = foldr1 mappend ops
  res = appEndo run ini
  cmp = serverCompare `on` points
  in maximumBy cmp res

prison::Game
prison Break Break = Points   0   0
prison Break Keep  = Points   2 (-1)
prison Keep  Break = Points (-1)  2
prison Keep  Keep  = Points   1   1

main :: IO ()
main = do
  [count] <- fmap (map read) getArgs
  let strat@Strategy{..} = serverChoice prison count
  putStr "server gains "
  print $ server points
  putStr "client gains "
  print $ client points
  putStr "client strategy: "
  putStrLn $ intercalate " -> " $ map show clientStrat
  putStrLn "server strategy:"
  putStr $ drawStrat strat

drawStrat = drawTree . go "at first" where
  go prefix Strategy{..} = Node (prefix ++ ": " ++ show choice) $ catMaybes [
      go "if client keeping"   <$> keepStrat,
      go "if client breaking"  <$> breakStrat
    ]
