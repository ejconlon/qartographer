{-# LANGUAGE OverloadedStrings #-}

module Qartographer.Core.Typing where

import qualified Data.GraphQL.AST as G
import qualified Data.HashMap.Strict as HMS
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Qartographer.Core.Validation

data Reason =
    TypeNotFound Text
  deriving (Show, Eq)

type VR a = Validation [Reason] a

type Schema = HashMap Text G.TypeDefinition

data Primitive = IntPrim
               | FloatPrim
               | BoolPrim
               | StringPrim
               deriving (Show, Eq)

data TypeDef = PrimDef Primitive
             | UserDef G.TypeDefinition
             deriving (Show, Eq)

lookupDef :: Text -> Schema -> VR TypeDef
lookupDef name schema =
  case name of
    "Int" -> pure (PrimDef IntPrim)
    "Float" -> pure (PrimDef FloatPrim)
    "Bool" -> pure (PrimDef BoolPrim)
    "String" -> pure (PrimDef StringPrim)
    _ ->
      case HMS.lookup name schema of
        Nothing -> invalidF (TypeNotFound name)
        Just def -> pure (UserDef def)

documentToSchema :: G.Document -> VR Schema
documentToSchema = undefined

isValidSchema :: Schema -> VR ()
isValidSchema = undefined

isServableSchema :: Schema -> VR ()
isServableSchema = undefined

isValidQuery :: G.Document -> VR ()
isValidQuery = undefined

canQuerySchema :: G.Document -> Schema -> VR ()
canQuerySchema = undefined

-- TODO figure out answers set
-- answersQuery :: G.Value -> G.Document -> VR ()
