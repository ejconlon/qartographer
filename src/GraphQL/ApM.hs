{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module GraphQL.ApM
  ( ApM(..)
  , liftApM
  , bindApM
  , runApM
  ) where

import Control.Monad (join)

data ApM f g a where
  Pure :: g a -> ApM f g a
  Ap   :: f a -> ApM f g (a -> g b) -> ApM f g b
  Flat :: ApM f g (g a) -> ApM f g a

instance Functor g => Functor (ApM f g) where
  fmap f (Pure ga) = Pure (f <$> ga)
  fmap f (Ap x y)  = Ap x (fmap ((fmap f) .) y)

conv :: Applicative g => g (a -> b) -> a -> g b
conv gf a = gf <*> pure a

instance Applicative g => Applicative (ApM f g) where
  pure = Pure . pure
  Pure gf <*> Pure a = Pure (gf <*> a)
  Pure gf <*> Flat x = Flat (fmap (gf <*>) x)
  Pure gf <*> Ap x y = Ap x (fmap (\zToA -> \z -> gf <*> (zToA z)) y)
  Flat x <*> y = Flat ((conv <$> x) <*> y)
  Ap x y <*> z = Ap x (flip <$> (fmap (fmap conv) y) <*> z)

liftApM :: Applicative g => f a -> ApM f g a
liftApM x = Ap x (Pure . pure $ pure)

bindApM :: Functor g => ApM f g a -> (a -> g b) -> ApM f g b
bindApM x f = Flat (f <$> x)

joining :: (Monad h, Applicative g) => (forall x. g (h x) -> h x) -> h (g x) -> h x
joining transGH = join . (fmap (transGH . fmap return))

runApM :: (Monad h, Applicative g) => (forall x. f x -> h x) -> (forall x. g (h x) -> h x) -> ApM f g a -> h a
runApM _ transGH (Pure ga) = transGH (fmap return ga)
runApM transFH transGH (Flat under) = joining transGH $ runApM transFH transGH under
runApM transFH transGH (Ap x y) = 
  let y' = runApM transFH transGH y
      x' = transFH x
  in joining transGH $ y' <*> x'
