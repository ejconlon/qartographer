{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Qartographer.Server.Core

import           Control.Monad.State
import qualified Data.GraphQL.AST              as G
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HMS
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Qartographer.Client.Http
import           Qartographer.Core.Typing
import           Qartographer.Core.Validation
import           Qartographer.Integration.Http
import           Qartographer.Server.Http
import           Test.Tasty
import           Test.Tasty.HUnit
import           Web.Spock

testParsing :: TestTree
testParsing = testCase "Parsing" $ do
  let a0 = runValidation $ parseTypeDefs "type Foo { bar: String }"
      f0 = G.FieldDefinition "bar" [] (G.TypeNamed (G.NamedType "String"))
      e0 = Right [G.TypeDefinitionObject (G.ObjectTypeDefinition "Foo" [] [f0])]
  a0 @?= e0
  let a1 = runValidation $ parseTypeDefs "type Foo { bar(baz: Int = 1): String }"
      g1 = G.InputValueDefinition "baz" (G.TypeNamed (G.NamedType "Int")) (Just (G.ValueInt 1))
      f1 = G.FieldDefinition "bar" [g1] (G.TypeNamed (G.NamedType "String"))
      e1 = Right [G.TypeDefinitionObject (G.ObjectTypeDefinition "Foo" [] [f1])]
  a1 @?= e1

runSpec :: HashMap Text Schema -> Client a -> IO a
runSpec schemas client = do
  app <- spockAsApp (makeMiddleware schemas)
  runApplication 6969 app client

testIntegration :: TestTree
testIntegration = testCase "Integration" $ do
  let schemas = HMS.empty
  runSpec schemas $ do
    _ <- clientGet "hello/world"
    liftIO $ 1 @?= 1

-- data BaseState = BaseState deriving (Eq, Show)

-- newtype BaseM a = BaseM
--   { unBaseM :: State BaseState a
--   } deriving (Functor, Applicative, Monad, MonadState BaseState)

tests :: TestTree
tests = testGroup "Tests"
  [ testParsing
  , testIntegration
  ]

main :: IO ()
main = defaultMain tests
