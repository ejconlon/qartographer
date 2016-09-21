{-# LANGUAGE OverloadedStrings #-}

module Qartographer.Core.Typing where

import qualified Data.GraphQL.AST as G
import qualified Data.GraphQL.Encoder as GE
import qualified Data.HashMap.Strict as HMS
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Qartographer.Core.Validation

data Reason =
    TypeNotFound Text
  deriving (Show, Eq)

type VR a = Validation [Reason] a

type TypeMap = HashMap Text G.TypeDefinition

data Schema = Schema
  { _schemaQueryTypeName :: Text
  , _schemaMutationTypeName :: Maybe Text
  , _schemaSubscriptionTypeName :: Maybe Text
  , _schemaTypes :: TypeMap
  --, _schemaDirectives :: Directive
  } deriving (Show, Eq)

makeObject :: [(G.Name, G.Value)] -> G.Value
makeObject = undefined

makeString :: Text -> G.Value
makeString = G.ValueString . G.StringValue

makeNamed :: G.Name -> Text -> [(G.Name, G.Value)]
makeNamed nn n = [(nn, makeObject [("name", makeString n)])]

renderSchema :: Schema -> G.Value
renderSchema (Schema qtn mtn stn types) =
  makeObject $
    (makeNamed "queryType" qtn)
    ++ (maybe [] (makeNamed "mutationType") mtn)
    ++ (maybe [] (makeNamed "subscriptionType") stn)

parseTypeDefs :: Text -> Either String [G.TypeDefinition]
parseTypeDefs = undefined

renderTypeDefs :: [G.TypeDefinition] -> Text
renderTypeDefs defs = GE.document (G.Document (G.DefinitionType <$> defs))

data Qdoc =
    OpQdoc G.OperationDefinition
  | FragQdoc G.FragmentDefinition
  deriving (Show, Eq)

qdocToDef :: Qdoc -> G.Definition
qdocToDef (OpQdoc op) = G.DefinitionOperation op
qdocToDef (FragQdoc frag) = G.DefinitionFragment frag

parseQdocs :: Text -> Either String [Qdoc]
parseQdocs = undefined

renderQdocs :: [Qdoc] -> Text
renderQdocs qdocs = GE.document (G.Document (qdocToDef <$> qdocs))

data Primitive = IntPrim
               | FloatPrim
               | BoolPrim
               | StringPrim
               deriving (Show, Eq)

data TypeDef = PrimDef Primitive
             | UserDef G.TypeDefinition
             deriving (Show, Eq)

lookupDef :: Text -> TypeMap -> VR TypeDef
lookupDef name tmap =
  case name of
    "Int" -> pure (PrimDef IntPrim)
    "Float" -> pure (PrimDef FloatPrim)
    "Bool" -> pure (PrimDef BoolPrim)
    "String" -> pure (PrimDef StringPrim)
    _ ->
      case HMS.lookup name tmap of
        Nothing -> invalidF (TypeNotFound name)
        Just def -> pure (UserDef def)

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
