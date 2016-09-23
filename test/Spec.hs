{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes #-}

import           Qartographer.Server.Core

import           Control.Monad.State
import Data.Either (isRight)
import qualified Data.GraphQL.AST              as G
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HMS
import           Data.HashSet                  (HashSet)
import qualified Data.HashSet                  as HS
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Qartographer.Client.Http
import           Qartographer.Core.Typing
import           Qartographer.Core.Validation
import           Qartographer.Integration.Http
import           Qartographer.Server.Http
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Heredoc (here)
import           Web.Spock (spockAsApp)

example :: Text
example = T.pack [here|
enum DogCommand { SIT, DOWN, HEEL }

type Dog implements Pet {
  name: String!
  nickname: String
  barkVolume: Int
  doesKnowCommand(dogCommand: DogCommand!): Boolean!
  isHousetrained(atOtherHomes: Boolean): Boolean!
  owner: Human
}

interface Sentient {
  name: String!
}

interface Pet {
  name: String!
}

type Alien implements Sentient {
  name: String!
  homePlanet: String
}

type Human implements Sentient {
  name: String!
}

enum CatCommand { JUMP }

type Cat implements Pet {
  name: String!
  nickname: String
  doesKnowCommand(catCommand: CatCommand!): Boolean!
  meowVolume: Int
}

union CatOrDog = Cat | Dog
union DogOrHuman = Dog | Human
union HumanOrAlien = Human | Alien

type QueryRoot {
  dog: Dog
}
|]

expectedTopLevelNames :: HashSet Text
expectedTopLevelNames = HS.fromList
  [ "DogCommand"
  , "Dog"
  , "Sentient"
  , "Pet"
  , "Alien"
  , "Human"
  , "CatCommand"
  , "Cat"
  , "CatOrDog"
  , "DogOrHuman"
  , "HumanOrAlien"
  , "QueryRoot"
  ]

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

testValidation :: TestTree
testValidation = testCase "Validation" $ do
  let a0 = runValidation $ parseTypeDefs example
      (Right defs) = a0
      tymap = makeTypeMap defs
  HS.fromList (HMS.keys tymap) @?= expectedTopLevelNames
  let schema = Schema "QueryRoot" Nothing Nothing tymap
      a1 = runValidation $ validateSchema schema
      e1 = Right ()
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
  , testValidation
  -- , testIntegration
  ]

main :: IO ()
main = defaultMain tests
