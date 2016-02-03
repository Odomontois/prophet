
{-# LANGUAGE RecordWildCards, DeriveDataTypeable, TypeFamilies, ViewPatterns #-}

module Main where

import System.Console.CmdArgs
import Data.List
import Data.Tree
import Data.Maybe
import Game
import Control.Monad
import Data.Monoid
import DecisionTree

data Params = Params {count::Int, clientSkip::Bool, serverSkip::Bool, treeSkip::Bool }
  deriving (Data,Typeable,Show,Eq)

params::Params
params = Params {
  count      = 5     &= name "n"  &= help "games count",
  clientSkip = False &= help "skip client strategy output",
  serverSkip = False &= help "skip server strategy output",
  treeSkip   = False &= help "skip tree output"
}

data Prison = Break | Keep deriving (Show, Eq, Ord, Enum, Bounded)

instance Game Prison where
  type Interest Prison = Sum Integer

  game Break Break = ( 0,  0)
  game Break Keep  = ( 2 ,-1)
  game Keep  Break = (-1,  2)
  game Keep  Keep  = ( 1,  1)

  choices = [Break, Keep]

  mkChoicer [break, keep] = choose where
    choose Keep  = keep
    choose Break = break

labelledOut::String->String->IO ()
labelledOut label val = putStr label >> putStrLn val

main :: IO ()
main = do
  Params{..} <- cmdArgs params
  let strat = serverChoice count
  let Move{..} = extract strat
  putStr "count is "
  print count
  labelledOut "server gains " $ show $ getSum $ server points
  labelledOut "client gains " $ show $ getSum $ client points
  unless clientSkip $ labelledOut "client strategy: " $ showStrat $ clientStrat strat
  unless serverSkip $ labelledOut "server strategy: " $ showStrat $ serverStrat strat
  unless treeSkip   $ labelledOut "server strategy tree:\n" $ drawStrat strat

showStrat::Show a=>[a]->String
showStrat = intercalate " -> " . map show

drawStrat = drawTree . go "at first" where
  go prefix (Move{..} :< (unwrapDT -> subStrat)) = Node (prefix ++ ": " ++ show (server choice)) $ catMaybes [
      go "if client keeping"   <$> subStrat Keep,
      go "if client breaking"  <$> subStrat Break
    ]
