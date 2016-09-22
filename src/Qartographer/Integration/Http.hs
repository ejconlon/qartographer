{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Qartographer.Integration.Http where

import           Control.Concurrent
import           Control.Exception
import           Network.HTTP.Client
import           Network.Wai
import           Network.Wai.Handler.Warp as W
import           Network.Wreq.Session
import           Qartographer.Client.Http
import           Qartographer.Server.Http

type WithClient a = Int -> Client a -> IO a

withClient :: WithClient a
withClient port client =
  let callback = runClient client . localClient port
      cookieJar = createCookieJar []
      manager = defaultManagerSettings
  in withSessionControl (Just cookieJar) manager callback

runApplication :: Int -> Application -> Client a -> IO a
runApplication port app client = do
  bracket (forkIO $ W.run port app) (killThread) $ \_ -> do
    threadDelay 10000
    withClient port client
