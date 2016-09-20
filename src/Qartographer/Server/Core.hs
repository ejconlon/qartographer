{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Qartographer.Server.Core where

import qualified Data.GraphQL.AST as G
import Control.Applicative.Free
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Writer hiding ((<>))
import Data.Int
import Data.Semigroup (Semigroup(..))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Typeable
import GHC.Exception

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

invalid :: e -> Validation e a
invalid = Validation . Left

data HandlerEnv = HandlerEnv
  { _handlerEnvArgs :: [(G.Name, G.Value)]
  , _handlerSelSet  :: G.SelectionSet
  }

data HandlerError =
    NoArgError G.Name
  | BadArgTypeError G.Name G.Type G.Value
  deriving (Eq, Show, Typeable)
instance Exception HandlerError

newtype HandlerT m a = HandlerT
  { unHandlerT :: ReaderT HandlerEnv m a
  } deriving (Functor, Applicative, Monad, MonadReader HandlerEnv, MonadThrow)

instance MonadTrans HandlerT where
  lift = HandlerT . lift

instance Monad m => MonadBase m (HandlerT m) where
  liftBase = lift

runHandlerT :: Monad m =>  HandlerT m a -> HandlerEnv -> m a
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

data FieldDecl e m a = FieldDecl
  { _fieldDeclField :: G.FieldDefinition
  , _fieldDeclHandler :: HandlerT m (Validation e a)
  }

declareFieldFn :: MonadThrow m => G.Name -> G.Type -> Args a -> (a -> HandlerT m (Validation e a)) -> FieldDecl e m a
declareFieldFn name retTy args makeBody =
  let defs = argDefs args
      field = G.FieldDefinition name defs retTy
      handler = makeHandler args makeBody
  in FieldDecl field handler

declareFieldFn0 :: G.Name -> G.Type -> HandlerT m (Validation e a) -> FieldDecl e m a
declareFieldFn0 name retTy body =
  let field = G.FieldDefinition name [] retTy
      handler = restrictTo [] body
  in FieldDecl field handler

-- declareFieldObject :: G.Name -> ObjectDecl m -> FieldDecl m
-- declareFieldObject name 

data ObjectDecl e m a = ObjectDecl
  { _objectDeclName :: G.Name
  , _objectDeclFieldDecls :: [FieldDecl e m a]
  }

objTy :: ObjectDecl e m a -> G.Type
objTy = undefined

objHandler :: ObjectDecl e m a -> HandlerT m (Validation e a)
objHandler = undefined
