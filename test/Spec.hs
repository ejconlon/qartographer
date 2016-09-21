{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

import           Qartographer.Server.Core

import           Control.Monad.State
import qualified Data.Attoparsec.Text     as AT
import qualified Data.GraphQL.AST         as G
import qualified Data.GraphQL.Parser      as GP
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Heredoc             (str)

-- Parsing utils

runParser :: AT.Parser a -> Text -> Either String a
runParser parser = AT.parseOnly (parser <* AT.endOfInput)

-- Now actual tests

testParsing :: TestTree
testParsing = testCase "Parsing" $ do
  let a0 = runParser GP.typeDefinition "type Foo { bar: String }"
      f0 = G.FieldDefinition "bar" [] (G.TypeNamed (G.NamedType "String"))
      e0 = Right (G.TypeDefinitionObject (G.ObjectTypeDefinition "Foo" [] [f0]))
  a0 @?= e0
  let a1 = runParser GP.typeDefinition "type Foo { bar(baz: Int = 1): String }"
      g1 = G.InputValueDefinition "baz" (G.TypeNamed (G.NamedType "Int")) (Just (G.ValueInt 1))
      f1 = G.FieldDefinition "bar" [g1] (G.TypeNamed (G.NamedType "String"))
      e1 = Right (G.TypeDefinitionObject (G.ObjectTypeDefinition "Foo" [] [f1]))
  a1 @?= e1

data BaseState = BaseState deriving (Eq, Show)

newtype BaseM a = BaseM
  { unBaseM :: State BaseState a
  } deriving (Functor, Applicative, Monad, MonadState BaseState)

tests :: TestTree
tests = testGroup "Tests"
  [ testParsing
  ]

main :: IO ()
main = defaultMain tests
