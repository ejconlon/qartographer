{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module GraphQL.Server where

import qualified Data.GraphQL.AST as G
import Control.Applicative.Free
import Control.Lens
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Writer
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
  , _handlerEnvArgs :: [(G.Name, G.Value)]
  }

data HandlerError =
    NoArgError G.Name
  | BadArgTypeError G.Name G.Type G.Value
  deriving (Eq, Show, Typeable)
instance Exception HandlerError

newtype HandlerT m a = HandlerT
  { unHandlerT :: ReaderT (HandlerEnv m) m a
  } deriving (Functor, Applicative, Monad, MonadReader (HandlerEnv m), MonadThrow)

instance MonadTrans HandlerT where
  lift = HandlerT . lift

instance Monad m => MonadBase m (HandlerT m) where
  liftBase = lift

runHandlerT :: Monad m =>  HandlerT m a -> HandlerEnv m -> m a
runHandlerT = runReaderT . unHandlerT

data ArgTy a where
  StringArgTy :: ArgTy Text
  IntArgTy :: ArgTy Int32

stringTy :: G.Type
stringTy = G.TypeNamed $ G.NamedType "String"

intTy :: G.Type
intTy = G.TypeNamed $ G.NamedType "Int"

extTy :: ArgTy a -> G.Type
extTy StringArgTy = stringTy
extTy IntArgTy = intTy

data ArgDef a = ArgDef
  { _argDefName :: G.Name
  , _argDefDefault :: Maybe G.Value
  , _argDefType :: ArgTy a
  }

extDef :: ArgDef a -> G.InputValueDefinition
extDef (ArgDef name def ty) = G.InputValueDefinition name (extTy ty) def

type Args a = Ap ArgDef a

argDefs :: Args a -> G.ArgumentsDefinition
argDefs = runAp_ (return . extDef)

projectString :: G.Value -> Maybe Text
projectString (G.ValueString (G.StringValue t)) = Just t
projectString _ = Nothing

projectInt :: G.Value -> Maybe Int32
projectInt (G.ValueInt i) = Just i
projectInt _ = Nothing

projectTy :: ArgTy a -> G.Value -> Maybe a
projectTy argTy =
  case argTy of
    StringArgTy -> projectString
    IntArgTy -> projectInt

-- ~ natural transformation
lookupArg :: MonadThrow m => ArgDef a -> HandlerT m a
lookupArg (ArgDef name def argTy) = do
  args <- asks _handlerEnvArgs
  case lookup name args of
    Nothing -> throwM $ NoArgError name
    Just value ->
      case projectTy argTy value of
        Nothing -> throwM $ BadArgTypeError name (extTy argTy) value
        Just parsed -> return parsed

-- TODO actually restrict
restrictTo :: G.ArgumentsDefinition -> HandlerT m a -> HandlerT m a
restrictTo args act = act

interpret :: MonadThrow m => Args a -> HandlerT m a
interpret = runAp lookupArg

makeHandler :: MonadThrow m => Args a -> (a -> HandlerT m b) -> HandlerT m b
makeHandler args makeBody = do
  let defs = argDefs args
  parsed <- interpret args
  let body = makeBody parsed
  restrictTo defs body

data FieldDecl m = FieldDecl
  { _fieldDeclField :: G.FieldDefinition
  , _fieldDeclHandler :: HandlerT m G.Value
  }

newtype FieldDeclT m a = FieldDeclT
  { unFieldDeclT :: WriterT [FieldDecl m] m a 
  } deriving (Functor, Applicative, Monad, MonadWriter [FieldDecl m], MonadThrow)

instance MonadTrans FieldDeclT where
  lift = FieldDeclT . lift

instance Monad m => MonadBase m (FieldDeclT m) where
  liftBase = lift

runFieldDeclT :: Monad m => FieldDeclT m a -> m (a, [FieldDecl m])
runFieldDeclT = runWriterT . unFieldDeclT

declareField :: MonadThrow m => G.Name -> G.Type -> Args a -> (a -> HandlerT m G.Value) -> FieldDeclT m ()
declareField name retTy args makeBody =
  let defs = argDefs args
      field = G.FieldDefinition name defs retTy
      handler = makeHandler args makeBody
      decl = FieldDecl field handler
  in tell [decl]

declareField0 :: Monad m => G.Name -> G.Type -> HandlerT m G.Value -> FieldDeclT m ()
declareField0 name retTy body =
  let field = G.FieldDefinition name [] retTy
      handler = restrictTo [] body
      decl = FieldDecl field handler
  in tell [decl]
