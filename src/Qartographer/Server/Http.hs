{-# LANGUAGE OverloadedStrings #-}

module Qartographer.Server.Http where

import Control.Monad (forM_)
import qualified Data.HashMap.Strict as HMS
import Data.HashMap.Strict (HashMap)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.Wai.Middleware.Static (hasPrefix, staticPolicy)
import Prelude hiding (head)
import Web.Spock
import Web.Spock.Config
import Qartographer.Core.Typing
import System.Environment (getArgs)

testSchema :: Schema
testSchema = Schema "Foo" Nothing Nothing HMS.empty

httpMain :: IO ()
httpMain = do
  args <- getArgs
  port <-
    case args of
      [] -> return 8080
      [p] -> return (read p :: Int)
      _ -> fail "Expected [port] or nothing for [8080]"
  let schemas = HMS.fromList [("test", testSchema)]
  serve port schemas

data AppSession = AppSession

data AppState = AppState

serve :: Int -> HashMap Text Schema -> IO ()
serve port schemas = do
  spockCfg <- defaultSpockCfg AppSession PCNoDatabase AppState
  runSpock port (spock spockCfg (app schemas))

contentTypeGraphQL :: Text
contentTypeGraphQL = "application/graphql; charset=utf-8"

app :: HashMap Text Schema -> SpockM () AppSession AppState ()
app schemas = do
  middleware (staticPolicy (hasPrefix "static"))
  get ("hello" <//> var) $ \name -> do
    text $ "hello " <> name
  subcomponent "schemas" $ do
    forM_ (HMS.toList schemas) $ \(name, schema) -> do
      subcomponent (static (T.unpack name)) $ do
        get "types" $ do
          setHeader "Content-Type" contentTypeGraphQL
          bytes $ TE.encodeUtf8 $ renderTypeDefs $ HMS.elems $ _schemaTypes schema
