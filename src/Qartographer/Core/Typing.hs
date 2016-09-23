{-# LANGUAGE OverloadedStrings #-}

module Qartographer.Core.Typing where

import           Control.Applicative          ((<|>))
import           Control.Monad                (unless)
import           Data.Aeson                   ((.=))
import qualified Data.Aeson                   as A
import qualified Data.Attoparsec.Text         as AT
import           Data.Foldable                (for_, sequenceA_)
import qualified Data.GraphQL.AST             as G
import qualified Data.GraphQL.Encoder         as GE
import qualified Data.GraphQL.Parser          as GP
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HMS
import Data.List (foldl')
import           Data.Maybe                   (maybeToList)
import           Data.Monoid                  ((<>))
import           Data.HashSet                     (HashSet)
import qualified Data.HashSet                     as HS
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Qartographer.Core.Validation

data Reason =
    TypeNotFound Text
  | ParserError Text
  deriving (Show, Eq)

type VR a = Validation [Reason] a

fromParseResult :: Either String a -> VR a
fromParseResult (Left e)  = invalidF $ ParserError $ T.pack e
fromParseResult (Right a) = pure a

type TypeMap = HashMap Text G.TypeDefinition

data Schema = Schema
  { _schemaQueryTypeName        :: Text
  , _schemaMutationTypeName     :: Maybe Text
  , _schemaSubscriptionTypeName :: Maybe Text
  , _schemaTypes                :: TypeMap
  --, _schemaDirectives :: Directive
  } deriving (Show, Eq)

makeNamed :: G.Name -> Text -> [(Text, A.Value)]
makeNamed nn n = [(nn, A.object ["name" .= n])]

instance A.ToJSON Schema where
  toJSON (Schema qtn mtn stn types) =
    A.object $
      (makeNamed "queryType" qtn)
      ++ (maybe [] (makeNamed "mutationType") mtn)
      ++ (maybe [] (makeNamed "subscriptionType") stn)

parseTypeDefs :: Text -> VR [G.TypeDefinition]
parseTypeDefs = fromParseResult . AT.parseOnly (parser <* AT.endOfInput)
  where parser = GP.whiteSpace *> AT.many1 GP.typeDefinition

renderTypeDefs :: [G.TypeDefinition] -> Text
renderTypeDefs defs = GE.document (G.Document (G.DefinitionType <$> defs))

-- TODO
typeNameOf :: G.TypeDefinition -> Maybe Text
typeNameOf (G.TypeDefinitionObject (G.ObjectTypeDefinition name _ _)) = Just name
typeNameOf (G.TypeDefinitionInterface (G.InterfaceTypeDefinition name _)) = Just name
typeNameOf (G.TypeDefinitionUnion (G.UnionTypeDefinition name _)) = Just name
typeNameOf (G.TypeDefinitionScalar (G.ScalarTypeDefinition name)) = Just name
typeNameOf (G.TypeDefinitionEnum (G.EnumTypeDefinition name _)) = Just name
typeNameOf _ = Nothing

makeTypeMap :: [G.TypeDefinition] -> TypeMap
makeTypeMap defs = HMS.fromList $ do
  def <- defs
  case typeNameOf def of
    Nothing -> []
    Just name -> [(name, def)]

data Qdoc =
    OpQdoc G.OperationDefinition
  | FragQdoc G.FragmentDefinition
  deriving (Show, Eq)

qdocToDef :: Qdoc -> G.Definition
qdocToDef (OpQdoc op)     = G.DefinitionOperation op
qdocToDef (FragQdoc frag) = G.DefinitionFragment frag

parseQdocs :: Text -> VR [Qdoc]
parseQdocs = fromParseResult . AT.parseOnly (parser <* AT.endOfInput)
  where parser = GP.whiteSpace *> AT.many1 (opParser <|> fragParser)
        opParser = OpQdoc <$> GP.operationDefinition
        fragParser = FragQdoc <$> GP.fragmentDefinition

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
    "Boolean" -> pure (PrimDef BoolPrim)
    "String" -> pure (PrimDef StringPrim)
    _ ->
      case HMS.lookup name tmap of
        Nothing  -> invalidF (TypeNotFound name)
        Just def -> pure (UserDef def)

namesInType :: G.Type -> HashSet Text
namesInType (G.TypeNamed named) = namesInNamedDef named
namesInType (G.TypeList (G.ListType ty)) = namesInType ty
namesInType (G.TypeNonNull (G.NonNullTypeNamed named)) = namesInNamedDef named
namesInType (G.TypeNonNull (G.NonNullTypeList (G.ListType ty))) = namesInType ty

namesInFieldDef :: G.FieldDefinition -> HashSet Text
namesInFieldDef (G.FieldDefinition _ args ty) =
  mconcat (namesInInputValueDef <$> args) <>
    namesInType ty

namesInNamedDef :: G.NamedType -> HashSet Text
namesInNamedDef (G.NamedType name) = HS.singleton name

namesInObjectDef :: G.ObjectTypeDefinition -> HashSet Text
namesInObjectDef (G.ObjectTypeDefinition name interfaces fields) =
  HS.singleton name <>
    mconcat (namesInNamedDef <$> interfaces) <>
    mconcat (namesInFieldDef <$> fields)

namesInInterfaceDef :: G.InterfaceTypeDefinition -> HashSet Text
namesInInterfaceDef (G.InterfaceTypeDefinition name fields) =
  HS.singleton name <>
    mconcat (namesInFieldDef <$> fields)

namesInUnionDef :: G.UnionTypeDefinition -> HashSet Text
namesInUnionDef (G.UnionTypeDefinition name variants) =
  HS.singleton name <>
    mconcat (namesInNamedDef <$> variants)

namesInScalarDef :: G.ScalarTypeDefinition -> HashSet Text
namesInScalarDef (G.ScalarTypeDefinition name) =
  HS.singleton name

namesInEnumDef :: G.EnumTypeDefinition -> HashSet Text
namesInEnumDef (G.EnumTypeDefinition name _) =
  HS.singleton name
  -- TODO are enum values effectively singleton types?

namesInInputValueDef :: G.InputValueDefinition -> HashSet Text
namesInInputValueDef (G.InputValueDefinition _ ty _) =
  namesInType ty

namesInInputObjectDef :: G.InputObjectTypeDefinition -> HashSet Text
namesInInputObjectDef (G.InputObjectTypeDefinition _ ivdefs) =
  mconcat (namesInInputValueDef <$> ivdefs)

namesInTypeExtension :: G.TypeExtensionDefinition -> HashSet Text
namesInTypeExtension (G.TypeExtensionDefinition obj) =
  namesInObjectDef obj

namesInTypeDef :: G.TypeDefinition -> HashSet Text
namesInTypeDef (G.TypeDefinitionObject x) = namesInObjectDef x
namesInTypeDef (G.TypeDefinitionInterface x) = namesInInterfaceDef x
namesInTypeDef (G.TypeDefinitionUnion x) = namesInUnionDef x
namesInTypeDef (G.TypeDefinitionScalar x) = namesInScalarDef x
namesInTypeDef (G.TypeDefinitionEnum x) = namesInEnumDef x
namesInTypeDef (G.TypeDefinitionInputObject x) = namesInInputObjectDef x
namesInTypeDef (G.TypeDefinitionTypeExtension x) = namesInTypeExtension x

namesInSchema :: Schema -> HashSet Text
namesInSchema (Schema qtn mtn stn types) =
  let start = HS.fromList (qtn : maybeToList mtn ++ maybeToList stn)
  in foldl (<>) start (namesInTypeDef <$> HMS.elems types)

validateSchemaNames :: Schema -> VR ()
validateSchemaNames schema@(Schema _ _ _ types) =
  for_ (namesInSchema schema) (flip lookupDef types)

validateSchema :: Schema -> VR ()
validateSchema schema =
  sequenceA_
    [ validateSchemaNames schema
    ]

isValidQuery :: G.Document -> VR ()
isValidQuery = undefined

canQuerySchema :: G.Document -> Schema -> VR ()
canQuerySchema = undefined

-- TODO figure out answers set
-- answersQuery :: G.Value -> G.Document -> VR ()
