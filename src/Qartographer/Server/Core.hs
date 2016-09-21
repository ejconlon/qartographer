{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Qartographer.Server.Core where

import           Control.Applicative.Free
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Writer         hiding ((<>))
import           Data.Functor.Compose         (Compose (..))
import qualified Data.GraphQL.AST             as G
import           Data.Int
import           Data.Semigroup               (Semigroup (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Typeable
import           GHC.Exception
import           Qartographer.Core.Validation

data HandlerEnv = HandlerEnv
  { _handlerEnvArgs :: [(G.Name, G.Value)]
  , _handlerSelSet  :: G.SelectionSet
  }

data HandlerError =
    NoArgError G.Name
  | BadArgTypeError G.Name G.Type G.Value
  deriving (Eq, Show)

type VH a = Validation [HandlerError] a

newtype HandlerT m a = HandlerT
  { unHandlerT :: ReaderT HandlerEnv m a
  } deriving (Functor, Applicative, Monad, MonadReader HandlerEnv)

instance MonadTrans HandlerT where
  lift = HandlerT . lift

instance Monad m => MonadBase m (HandlerT m) where
  liftBase = lift

runHandlerT :: Monad m => HandlerT m a -> HandlerEnv -> m a
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
extTy IntArgTy    = intTy

encodeGValue :: ArgTy a -> a -> G.Value
encodeGValue StringArgTy t = G.ValueString (G.StringValue t)
encodeGValue IntArgTy i    = G.ValueInt i

projectString :: G.Value -> Maybe Text
projectString (G.ValueString (G.StringValue t)) = Just t
projectString _                                 = Nothing

projectInt :: G.Value -> Maybe Int32
projectInt (G.ValueInt i) = Just i
projectInt _              = Nothing

projectTy :: ArgTy a -> G.Value -> Maybe a
projectTy argTy =
  case argTy of
    StringArgTy -> projectString
    IntArgTy    -> projectInt

data ArgDef a = ArgDef
  { _argDefName    :: G.Name
  , _argDefDefault :: Maybe G.Value
  , _argDefType    :: ArgTy a
  }

extDef :: ArgDef a -> G.InputValueDefinition
extDef (ArgDef name def ty) = G.InputValueDefinition name (extTy ty) def

type Args a = Ap ArgDef a

argDefs :: Args a -> G.ArgumentsDefinition
argDefs = runAp_ (return . extDef)

lookupArg :: Monad m => ArgDef a -> HandlerT m (VH a)
lookupArg (ArgDef name def argTy) = do
  args <- asks _handlerEnvArgs
  return $
    case lookup name args of
      Nothing -> invalidF (NoArgError name)
      Just value ->
        case projectTy argTy value of
          Nothing     -> invalidF (BadArgTypeError name (extTy argTy) value)
          Just parsed -> pure parsed

-- TODO actually restrict
restrictTo :: Monad m => G.ArgumentsDefinition -> HandlerT m a -> HandlerT m a
restrictTo args = local restricted
  where
    restricted = id

interpret :: Monad m => Args a -> HandlerT m (VH a)
interpret args = getCompose $ runAp (Compose . lookupArg) args

makeHandler :: Monad m => Args a -> (a -> HandlerT m (VH b)) -> HandlerT m (VH b)
makeHandler args makeBody =
  interpret args >>- restrictTo (argDefs args) . makeBody

data FieldDecl m a = FieldDecl
  { _fieldDeclField   :: G.FieldDefinition
  , _fieldDeclHandler :: HandlerT m (VH a)
  }

data ObjectDecl m a = ObjectDecl
  { _objectDeclName       :: G.Name
  , _objectDeclFieldDecls :: [FieldDecl m a]
  }

objFields :: ObjectDecl m a -> [G.FieldDefinition]
objFields (ObjectDecl _ fields) = _fieldDeclField <$> fields

objHandler :: ObjectDecl m a -> HandlerT m (VH a)
objHandler = undefined

declareFieldFn :: Monad m => G.Name -> G.Type -> Args a -> (a -> HandlerT m (VH b)) -> FieldDecl m b
declareFieldFn name retTy args makeBody =
  let defs = argDefs args
      field = G.FieldDefinition name defs retTy
      handler = makeHandler args makeBody
  in FieldDecl field handler

declareFieldFn0 :: Monad m => G.Name -> G.Type -> HandlerT m (VH b) -> FieldDecl m b
declareFieldFn0 name retTy body = declareFieldFn name retTy (pure ()) (const body)

-- declareFieldObj :: G.Name -> Args a -> (a -> ObjectDecl m b) -> FieldDecl m b
-- declareFieldObj name args decl =
--   let defs = argDefs args
--       field = G.FieldDefinition name defs (G.NamedType (_objectDeclName decl))
--       handler = restrictTo defs (objHandler decl)
--   in FieldDecl field handler

-- declareFieldObj0 :: G.Name -> ObjectDecl m b -> FieldDecl m b
-- declareFieldObj0 name decl = declareFieldObj name (pure ()) (const decl)
