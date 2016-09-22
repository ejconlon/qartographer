{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Qartographer.Integration.Http where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader
import Network.HTTP.Client
import Network.Wai
import Network.Wai.Handler.Warp as W
import Network.Wreq.Session
import Qartographer.Server.Http

data IntegrationEnv = IntegrationEnv
  { _integrationEnvPort :: Int
  , _integrationEnvSession :: Session
  } deriving (Show)

newtype Integration a = Integration
  { unIntegration :: ReaderT IntegrationEnv IO a
  } deriving (Functor, Applicative, Monad, MonadReader IntegrationEnv, MonadIO)

runIntegration :: Integration a -> Int -> Session -> IO a
runIntegration integration port sess =
  runReaderT (unIntegration integration) (IntegrationEnv port sess)

type WithIntegration a = Int -> Integration a -> IO a

withIntegration :: WithIntegration a
withIntegration port integration =
  let callback = runIntegration integration port
      cookieJar = createCookieJar []
      manager = defaultManagerSettings
  in withSessionControl (Just cookieJar) manager callback

runApplication :: Int -> Application -> Integration a -> IO a
runApplication port app integration = do
  bracket (forkIO $ W.run port app) (killThread) $ \_ -> do
    -- threadDelay 1000 
    withIntegration port integration
