
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Array.IArray
import Data.Array.Unboxed
import Data.List
import Data.Ord
import System.Environment
import Data.Foldable

import Generator

data Choice = Break | Keep deriving (Show, Ix, Eq, Ord, Enum, Bounded)

data Strategy i = Strategy (UArray i Bool) (Strategy (i, Choice)) | EndStrategy

type Game = Choice -> Choice -> (Integer, Integer)

ssize::Ix i=>Strategy i->Int
ssize EndStrategy = 0
ssize (Strategy _ next) = 1 + ssize next

(+.)::(Num a, Num b)=>(a,b)->(a,b)->(a,b)
(x, y) +. (a, b) = (x + a, b + y)

points::[Choice]->Strategy ()->Game->(Integer, Integer)
points choices strategy game = go choices strategy () where
  go::(Ix i)=>[Choice]->Strategy i->i->(Integer, Integer)
  go [] _ _ = (0,0)
  go _ EndStrategy _ = (0,0)
  go (choice: rest) (Strategy arr nextStrat) history = that +. next where
    that = game serv choice
    next = go rest nextStrat (history, choice)
    serv = toEnum . fromEnum $ arr ! history

clientSelect::Strategy ()->Game->((Integer, Integer), [Choice])
clientSelect strat game = let
  selections = do
      choices <- variations $ ssize strat
      return (points choices strat game, choices)
  clientPoints ((srv, clnt), _choice) = (clnt, - srv)
  in maximumBy (comparing clientPoints) selections

genStrategies::forall n.(Integral n, Eq n)=>n->IGen Bool (Strategy ())
genStrategies = genStratLevel () () 0

genStratLevel::forall n i.(Integral n, Eq n, Ix i)=>i->i->n->n->IGen Bool (Strategy i)
genStratLevel _ _ _ 0 = Single EndStrategy
genStratLevel from to k n = do
  let size = (2 ^ k)::n
  var <- variations size
  let arr = listArray (from, to) $ toList var
  next <- genStratLevel (from, Break) (to, Keep) (k + 1) (n - 1)
  return $ Strategy arr next

serverSelect::Game->Integer->(Integer, Integer, Strategy (), [Choice])
serverSelect game n = (server, client, strat, choice) where
  serverPoints = fst. fst . fst
  best = maximumBy (comparing serverPoints) $ do
    strategy <- genStrategies n
    return (clientSelect strategy game, strategy)
  (((server, client), choice), strat) = best

prison::Game
prison Break Break = ( 0,  0)
prison Break Keep  = ( 2, -1)
prison Keep  Break = (-1,  2)
prison Keep  Keep  = ( 1,  1)

main :: IO ()
main = do
  [count] <- fmap (map read) getArgs :: IO [Integer]
  print $ serverSelect prison count

class ShowHistory a where
  showHistory::a -> [String]
  showAssocs::Show b=>[(a, b)] -> String
  showAssocs as =  intercalate " ; " $ do
    (key, val) <- as
    let skey = intercalate "," $ showHistory key
    return (skey ++ "->" ++ show val)

instance ShowHistory () where showHistory _ = []
instance (ShowHistory a, Show b) => ShowHistory (a,b) where
  showHistory (a, b) = showHistory a ++ [show b]

instance (Show i, Ix i, ShowHistory i)=> Show (Strategy i) where
  show EndStrategy = ""
  show (Strategy arr next) = "{" ++ that ++ "}" ++ show next where
    that = showAssocs $ assocs arr
