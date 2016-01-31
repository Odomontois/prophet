
{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import Data.List
import Data.Tree
import Data.Maybe
import Strategy
import Control.Monad

data Params = Params {count::Int, clientSkip::Bool, serverSkip::Bool, treeSkip::Bool }
  deriving (Data,Typeable,Show,Eq)

params::Params
params = Params {
  count      = 5     &= name "n"  &= help "games count",
  clientSkip = False &= help "skip client strategy output",
  serverSkip = False &= help "skip server strategy output",
  treeSkip   = False &= help "skip tree output"
}

prison::Game
prison Break Break = Points   0   0
prison Break Keep  = Points   2 (-1)
prison Keep  Break = Points (-1)  2
prison Keep  Keep  = Points   1   1

labelledOut::String->String->IO ()
labelledOut label val = putStr label >> putStrLn val

main :: IO ()
main = do
  Params{..} <- cmdArgs params
  let strat@Strategy{..} = serverChoice prison count
  putStr "count is "
  print count
  labelledOut "server gains " $ show $ server points
  labelledOut "client gains " $ show $ client points
  unless clientSkip $ labelledOut "client strategy: " $ showStrat clientStrat
  unless serverSkip $ labelledOut "server strategy: " $ showStrat $ serverStrat strat
  unless treeSkip   $ labelledOut "server strategy tree:\n" $ drawStrat strat

showStrat::[Choice]->String
showStrat = intercalate " -> " . map show

drawStrat = drawTree . go "at first" where
  go prefix Strategy{..} = Node (prefix ++ ": " ++ show choice) $ catMaybes [
      go "if client keeping"   <$> subStrat Keep,
      go "if client breaking"  <$> subStrat Break
    ]
