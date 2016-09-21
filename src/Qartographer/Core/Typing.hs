module Qartographer.Core.Typing where

import qualified Data.GraphQL.AST as G
import qualified Data.HashMap.Strict as HMS
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Qartographer.Core.Validation

type TypeMap = HashMap Text G.TypeDefinition

isSubType :: TypeMap -> G.Type -> TypeMap -> G.Type -> Bool
isSubType lmap lty rmap rty =
  case (lty, rty) of
    (G.TypeNamed (G.NamedType lname), G.TypeNamed (G.NamedType rname)) -> undefined

isSubDefn :: TypeMap -> G.TypeDefinition -> TypeMap -> G.TypeDefinition -> Bool
isSubDefn lmap lty rmap rty = undefined