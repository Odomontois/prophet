{-#LANGUAGE GADTs#-}

module Generator where

import Control.Monad

data Generator t a b where
  Single::b->Generator t a b
  Layer::Foldable t=>t a->(a->Generator t a b)->Generator t a b

type LGen = Generator []

gen::Foldable t=>t a->Generator t a a
gen = flip Layer Single

liftGen::Foldable t=>Generator t a b->Generator (Generator t a) b b
liftGen = flip Layer Single

instance Functor (Generator t a) where
  fmap f (Single x) = Single (f x)
  fmap f (Layer els make) = Layer els (fmap f.make)

flatten::Generator t a (Generator t a b)->Generator t a b
flatten (Single g) = g
flatten (Layer els make) = Layer els (flatten.make)

instance Applicative (Generator t a) where
  pure = Single
  (<*>) = ap

instance Monad (Generator t a) where
  g >>= f = flatten . fmap f $ g

instance Foldable t => Foldable (Generator t a) where
  foldMap f (Single x) = f x
  foldMap f (Layer els make) = foldMap (foldMap f.make) els

variations :: (Bounded a, Enum a, Num n, Eq n)=>n->LGen [a] [a]
variations 0 = Single []
variations n = do
  [i] <- gen $ return <$> [minBound..maxBound]
  v <- variations (n - 1)
  return (i:v)
