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
import           Data.Maybe                   (maybeToList)
import           Data.Monoid                  ((<>))
import           Data.Set                     (Set)
import qualified Data.Set                     as S
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
    "Bool" -> pure (PrimDef BoolPrim)
    "String" -> pure (PrimDef StringPrim)
    _ ->
      case HMS.lookup name tmap of
        Nothing  -> invalidF (TypeNotFound name)
        Just def -> pure (UserDef def)

namesInTypeDef :: G.TypeDefinition -> Set Text
namesInTypeDef = undefined

namesInSchema :: Schema -> Set Text
namesInSchema (Schema qtn mtn stn types) =
  let start = S.fromList (qtn : maybeToList mtn ++ maybeToList stn)
  in foldl (<>) start (namesInTypeDef <$> HMS.elems types)

validateSchemaNames :: Schema -> VR ()
validateSchemaNames schema@(Schema _ _ _ types) =
  let names = namesInSchema schema
  in for_ names $ \name ->
    unless (HMS.member name types) (invalidF (TypeNotFound name))

validateSchema :: Schema -> VR ()
validateSchema schema =
  sequenceA_
    [ validateSchemaNames schema
    ]

isServableSchema :: Schema -> VR ()
isServableSchema = undefined

isValidQuery :: G.Document -> VR ()
isValidQuery = undefined

canQuerySchema :: G.Document -> Schema -> VR ()
canQuerySchema = undefined

-- TODO figure out answers set
-- answersQuery :: G.Value -> G.Document -> VR ()
