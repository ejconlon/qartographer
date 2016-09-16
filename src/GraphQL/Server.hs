{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module GraphQL.Server where

import qualified Data.GraphQL.AST as G
import Control.Lens
import Control.Monad (join)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Int
import qualified Data.Text as T
import Data.Text (Text)
import Data.Typeable
import GHC.Exception

data Resource m = Resource
  { _resourceDef :: G.ObjectTypeDefinition
  , _resourceResolve :: G.Name -> Root m -> m G.Value
  }

data Root m = Root
  { _rootResource :: G.Name -> m (Resource m)
  }

data HandlerEnv m = HandlerEnv
  { _handlerEnvRoot :: Root m
  , _handlerEnvArgs :: [(Text, G.Value)]
  }

data GraphQLError =
    NoArgError G.Name
  | BadArgTypeError G.Name G.Type G.Value
  deriving (Eq, Show, Typeable)
instance Exception GraphQLError

newtype HandlerT m a = HandlerT
  { unHandlerT :: ReaderT (HandlerEnv m) m a
  } deriving (Functor, Applicative, Monad, MonadReader (HandlerEnv m), MonadThrow)

instance MonadTrans HandlerT where
  lift = HandlerT . lift

instance Monad m => MonadBase m (HandlerT m) where
  liftBase = lift

lookupArg :: G.InputValueDefinition -> Handler m G.Value
lookupArg = undefined

runHandlerT :: Monad m =>  HandlerT m a -> HandlerEnv m -> m a
runHandlerT = runReaderT . unHandlerT

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
  ApErr x y <*> z = undefined

liftApErr :: f a -> ApErr e f a
liftApErr x = ApErr x (PureErr . Right $ Right)

throwApErr :: e -> ApErr e f a
throwApErr = PureErr . Left

tryApErr :: ApErr e f a -> (a -> Either e b) -> ApErr e f b
tryApErr x f = FlatErr (f <$> x)

runApErr :: Applicative g => (forall x. f x -> g x) -> ApErr e f a -> g (Either e a)
runApErr _ (PureErr ea) = pure ea
runApErr trans (FlatErr under) = join <$> runApErr trans under
runApErr trans (ApErr x y) = 
  let y' = convJoin <$> runApErr trans y
      x' = trans x
  in y' <*> x'

data ArgDef a = ArgDef G.InputValueDefinition deriving (Eq, Show)

type Args a = ApErr GraphQLError ArgDef a

-- TODO make sure to filter out args not defined with a local on the HandlerEnv
interpret :: Args a -> (G.ArgumentsDefinition, HandlerT m a)
interpret = undefined

baseArg :: G.Name -> G.Type -> Args G.Value
baseArg name ty = liftApErr $ ArgDef $ G.InputValueDefinition name ty Nothing

stringTy :: G.Type
stringTy = G.TypeNamed $ G.NamedType "String"

intTy :: G.Type
intTy = G.TypeNamed $ G.NamedType "Int"

projectString :: G.Name -> G.Value -> Either GraphQLError Text
projectString _ (G.ValueString (G.StringValue t)) = Right t
projectString name value = Left $ BadArgTypeError name stringTy value

projectInt :: G.Name -> G.Value -> Either GraphQLError Int32
projectInt _ (G.ValueInt i) = Right i
projectInt name value = Left $ BadArgTypeError name intTy value

stringArg :: G.Name -> Args Text
stringArg name = tryApErr (baseArg name stringTy) (projectString name)

intArg :: G.Name -> Args Int32
intArg name = tryApErr (baseArg name intTy) (projectInt name)
