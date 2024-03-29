{-# LANGUAGE DeriveFunctor #-}

module Qartographer.Core.Validation where

import Data.Either (isRight)
import           Data.Semigroup (Semigroup (..))

newtype Validation e a = Validation {
  runValidation :: Either e a
} deriving (Functor)

instance Semigroup e => Applicative (Validation e) where
  pure = Validation . Right
  (Validation x) <*> (Validation y) = Validation (z x y)
    where
      z (Left e1) (Left e2) = Left (e1 <> e2)
      z (Left e1) (Right _) = Left e1
      z (Right _) (Left e2) = Left e2
      z (Right f) (Right a) = Right (f a)

-- Not really a monad, since <*> != ap
-- instance Semigroup e => Monad (Validation e) where
--   return = pure
--   (Validation (Left es)) >>= _ = (Validation (Left es))
--   (Validation (Right a)) >>= f = f a

invalid :: e -> Validation e a
invalid = Validation . Left

invalidF :: Applicative f => e -> Validation (f e) a
invalidF = Validation . Left . pure

isValid :: Validation e a -> Bool
isValid (Validation e) = isRight e

-- Validation is not quite a monad. we want to be explicit about bind.
-- In particular, <*> != ap
(>>-) :: Monad m => m (Validation e a) -> (a -> m (Validation e b)) -> m (Validation e b)
mVal >>- f = do
  val <- mVal
  case runValidation val of
    Left es -> return $ invalid es
    Right a -> f a

infixr 8 >>-
