{-# LANGUAGE OverloadedStrings #-}

module Qartographer.Server.Http where

import Data.Monoid ((<>))
import Network.Wai.Middleware.Static
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
