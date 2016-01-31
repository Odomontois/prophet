
{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import Data.List
import Data.Tree
import Data.Maybe
import Strategy
import Control.Monad

data Params = Params {count::Int, clientSkip::Bool, serverSkip::Bool }
  deriving (Data,Typeable,Show,Eq)

params::Params
params = Params {
  count      = 5     &= name "n"  &= help "games count",
  clientSkip = False &= help "skip client strategy output",
  serverSkip = False &= help "skip server strategy output"
}

prison::Game
prison Break Break = Points   0   0
prison Break Keep  = Points   2 (-1)
prison Keep  Break = Points (-1)  2
prison Keep  Keep  = Points   1   1

main :: IO ()
main = do
  Params{..} <- cmdArgs params
  let strat@Strategy{..} = serverChoice prison count
  putStr "count is "
  print count
  putStr "server gains "
  print $ server points
  putStr "client gains "
  print $ client points
  unless clientSkip $ do
    putStr "client strategy: "
    putStrLn $ intercalate " -> " $ map show clientStrat
  unless serverSkip $ do
    putStrLn "server strategy:"
    putStr $ drawStrat strat

drawStrat = drawTree . go "at first" where
  go prefix Strategy{..} = Node (prefix ++ ": " ++ show choice) $ catMaybes [
      go "if client keeping"   <$> keepStrat,
      go "if client breaking"  <$> breakStrat
    ]
