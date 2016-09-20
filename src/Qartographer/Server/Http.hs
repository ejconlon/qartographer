{-# LANGUAGE OverloadedStrings #-}

module Qartographer.Server.Http where

import qualified Data.GraphQL.AST as GA
import qualified Data.GraphQL.Encoder as GE
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import Network.Wai.Middleware.Static
import Prelude hiding (head)
import Web.Spock
import Web.Spock.Config
import System.Environment (getArgs)

-- TODO get port and run server
httpMain :: IO ()
httpMain = do
  args <- getArgs
  (port, endpoint) <-
    case args of
      [] -> return (8080, "graphql")
      [p, e] -> return (read p :: Int, e)
      _ -> fail "Expected (port, endpoint) or nothing for (8080, graphql)"

  serve port endpoint

data AppSession = AppSession

data AppState = AppState

serve :: Int -> String -> IO ()
serve port endpoint = do
  spockCfg <- defaultSpockCfg AppSession PCNoDatabase AppState
  runSpock port (spock spockCfg (app endpoint))

app :: String -> SpockM () AppSession AppState ()
app endpoint = do
  middleware (staticPolicy (addBase "static"))
  get ("hello" <//> var) $ \name -> do
    text $ "hello " <> name

runGraphql :: GA.Document -> SpockM conn sess st ()
runGraphql doc = do
  head root $ do
    setHeader "Content-Type" "application/graphql; charset=utf-8"
    bytes $ TE.encodeUtf8 $ GE.document doc
