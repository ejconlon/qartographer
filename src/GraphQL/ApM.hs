{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module GraphQL.ApM where

import Control.Monad (join)

data ApErr e f a where
  PureErr :: Either e a -> ApErr e f a
  ApErr   :: f a -> ApErr e f (a -> Either e b) -> ApErr e f b
  FlatErr :: ApErr e f (Either e a) -> ApErr e f a

instance Functor (ApErr e f) where
  fmap f (PureErr ea) = PureErr (fmap f ea)
  fmap f (ApErr x y) = ApErr x (fmap ((fmap f) .) y)
  fmap f (FlatErr x) = FlatErr (fmap (fmap f) x)

conv :: Either e (a -> b) -> a -> Either e b
conv (Left e) _ = Left e
conv (Right f) a = Right (f a)

convJoin :: Either e (a -> Either e b) -> a -> Either e b
convJoin (Left e) _ = Left e
convJoin (Right f) a = f a

instance Applicative (ApErr e f) where
  pure = PureErr . Right
  PureErr (Right f) <*> y = f <$> y
  PureErr (Left e) <*> _ = PureErr (Left e)
  FlatErr x <*> y = FlatErr ((conv <$> x) <*> y)
  ApErr x y <*> z = ApErr x (flip <$> (fmap (fmap conv) y) <*> z)

liftApErr :: f a -> ApErr e f a
liftApErr x = ApErr x (PureErr . Right $ Right)

throwApErr :: Either e a -> ApErr e f a
throwApErr = PureErr

tryApErr :: ApErr e f a -> (a -> Either e b) -> ApErr e f b
tryApErr x f = FlatErr (f <$> x)

runApErr :: Applicative g => (forall x. f x -> g x) -> ApErr e f a -> g (Either e a)
runApErr _ (PureErr ea) = pure ea
runApErr trans (FlatErr under) = join <$> runApErr trans under
runApErr trans (ApErr x y) = 
  let y' = convJoin <$> runApErr trans y
      x' = trans x
  in y' <*> x'
