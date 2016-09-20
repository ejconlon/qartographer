{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Qartographer.Server.Core

import Control.Monad.Catch.Pure
import Control.Monad.State
import Test.Tasty
import Test.Tasty.HUnit

-- Utilities to work with exception equality:

isOkLike :: (Eq a, Show a) => Either SomeException a -> (a -> Assertion) -> Assertion
isOkLike (Left e) _ = fail $ "got fail " ++ show e
isOkLike (Right x) f = f x

isOk :: (Eq a, Show a) => Either SomeException a -> a -> Assertion
isOk v a = isOkLike v (@?= a)

isFailLike :: (Show a, Exception e, Eq e) => Either SomeException a -> (e -> Assertion) -> Assertion
isFailLike (Right x) _ = fail $ "got ok " ++ show x
isFailLike (Left z) f =
  case fromException z of
    Nothing -> fail $ "incompatible: " ++ show z
    Just x -> f x

isFail :: (Show a, Exception e, Eq e) => Either SomeException a -> e -> Assertion
isFail v e = isFailLike v (@?= e)

-- Now actual tests

data BaseState = BaseState deriving (Eq, Show)

newtype BaseM a = BaseM
  { unBaseM :: StateT BaseState Catch a
  } deriving (Functor, Applicative, Monad, MonadState BaseState, MonadThrow)

tests :: TestTree
tests = testGroup "Tests"
  [
  ]

main :: IO ()
main = defaultMain tests
