{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Qartographer.Client.Http where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.ByteString.Lazy   as BL
import           Network.HTTP.Client
import           Network.Wreq           (defaults)
import           Network.Wreq.Session
import           Network.Wreq.Types     (Options (..), Postable (..))
import           System.FilePath.Posix  ((</>))

data ClientEnv = ClientEnv
  { _clientEnvHostPort :: String
  , _clientEnvSession  :: Session
  } deriving (Show)

localClient :: Int -> Session -> ClientEnv
localClient port = ClientEnv ("http://localhost:" ++ (show port))

makeUrl :: MonadReader ClientEnv m => FilePath -> m String
makeUrl path = do
  hostPort <- asks _clientEnvHostPort
  return $ hostPort </> path

clientGet :: (MonadReader ClientEnv m, MonadIO m) => FilePath -> m (Response ByteString)
clientGet path = clientGetWith defaults path

clientGetWith :: (MonadReader ClientEnv m, MonadIO m) => Options -> FilePath -> m (Response ByteString)
clientGetWith opts path = do
  sess <- asks _clientEnvSession
  url <- makeUrl path
  liftIO $ getWith opts sess url

clientPost :: (MonadReader ClientEnv m, MonadIO m, Postable a) => FilePath -> a -> m (Response ByteString)
clientPost path payload = clientPostWith defaults path payload

clientPostWith :: (MonadReader ClientEnv m, MonadIO m, Postable a) => Options -> FilePath -> a -> m (Response ByteString)
clientPostWith opts path payload = do
  sess <- asks _clientEnvSession
  url <- makeUrl path
  liftIO $ postWith opts sess url payload

newtype Client a = Client
  { unClient :: ReaderT ClientEnv IO a
  } deriving (Functor, Applicative, Monad, MonadReader ClientEnv, MonadIO)

runClient :: Client a -> ClientEnv -> IO a
runClient = runReaderT . unClient
