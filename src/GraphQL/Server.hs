{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module GraphQL.Server where

import qualified Data.GraphQL.AST as G
import Control.Lens
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Int
import qualified Data.Text as T
import Data.Text (Text)
import Data.Typeable
import GHC.Exception
import GraphQL.ApM

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

data ArgDef a = ArgDef G.InputValueDefinition deriving (Eq, Show)

type Args a = ApM ArgDef (Either GraphQLError) a

-- TODO make sure to filter out args not defined with a local on the HandlerEnv
interpret :: Args a -> (G.ArgumentsDefinition, HandlerT m a)
interpret = undefined

baseArg :: G.Name -> G.Type -> Args G.Value
baseArg name ty = liftApM $ ArgDef $ G.InputValueDefinition name ty Nothing

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
stringArg name = bindApM (baseArg name stringTy) (projectString name)

intArg :: G.Name -> Args Int32
intArg name = bindApM (baseArg name intTy) (projectInt name)
