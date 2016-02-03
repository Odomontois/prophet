{-#LANGUAGE ViewPatterns#-}

module DecisionTree(
  collect, DecisionTree, wrapDT, unwrapDT,
  module Control.Comonad,
  module Control.Comonad.Cofree
) where
import Control.Comonad.Cofree
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Comonad

type WrapDT a = MaybeT (Reader a)
type DecisionTree a = Cofree (MaybeT (Reader a))

collect::(b->a)->DecisionTree a b->[b]
collect get = go where
  go (first :< (unwrapDT -> decide)) = first : next decide first
  next decide =  maybe [] go . decide . get

unwrapDT :: WrapDT a b -> a -> Maybe b
unwrapDT = runReader.runMaybeT

wrapDT::(a -> Maybe b)-> WrapDT a b
wrapDT = MaybeT . reader
