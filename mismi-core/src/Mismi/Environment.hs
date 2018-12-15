{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.Environment (
    Env
  , Region (..)
  , RegionError (..)
  , Debugging (..)
  , getRegionFromEnv
  , getDebugging
  , setDebugging
  , renderRegionError
  , discoverAWSEnv
  , discoverAWSEnvWithRegion
  , discoverAWSEnvRetry
  , discoverAWSEnvWithRegionRetry
  , catchAuthError
  , newMismiEnv
  ) where

import           Control.Lens ((.~))
import           Control.Monad.Catch (Handler (..), MonadCatch (..), MonadThrow (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.AWS (Credentials (..), Region (..))
import           Control.Monad.Trans.AWS (Env, envLogger, envRegion, newEnv)
import           Control.Monad.Trans.AWS (LogLevel (..), Logger, newLogger)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Control.Retry (RetryPolicyM, constantDelay, limitRetries, recovering)

import qualified Data.Text as T
import           Data.Typeable (Typeable)

import           Network.AWS.Auth (AuthError (..))
import           Network.AWS.Data (fromText)

import           P

import           System.Environment (lookupEnv)
import           System.IO (IO, stderr)

data RegionError =
    MissingRegion
  | UnknownRegion Text
  deriving (Eq, Show, Typeable)

data Debugging =
    DebugEnabled Logger
  | DebugDisabled


getRegionFromEnv :: (MonadIO m, MonadThrow m) => ExceptT RegionError m Region
getRegionFromEnv = do
  mr <- liftIO $ lookupEnv "AWS_DEFAULT_REGION"
  case mr of
    Nothing ->
      throwE MissingRegion
    Just a ->
      case fromText $ T.pack a of
        Left e ->
          throwE $ UnknownRegion (T.pack e)
        Right r ->
          pure r

getDebugging :: MonadIO m => m Debugging
getDebugging = do
  d <- liftIO $ lookupEnv "AWS_DEBUG"
  maybe
    (return DebugDisabled)
    (\s ->
      case T.pack s of
        "true" ->
          return . DebugEnabled =<< newLogger Trace stderr
        "1" ->
          return . DebugEnabled =<< newLogger Trace stderr
        _ ->
          return DebugDisabled)
    d

setDebugging :: Debugging -> Env -> Env
setDebugging d e =
  case d of
    DebugEnabled lgr ->
      e & envLogger .~ lgr
    DebugDisabled ->
      e

discoverAWSEnv :: ExceptT RegionError IO Env
discoverAWSEnv =
  discoverAWSEnvRetry $ limitRetries 1 <> constantDelay 200000

discoverAWSEnvWithRegion :: Region -> IO Env
discoverAWSEnvWithRegion r =
  flip discoverAWSEnvWithRegionRetry r $ limitRetries 1 <> constantDelay 200000

discoverAWSEnvRetry :: RetryPolicyM IO -> ExceptT RegionError IO Env
discoverAWSEnvRetry retry = do
  r <- getRegionFromEnv
  lift $ discoverAWSEnvWithRegionRetry retry r

discoverAWSEnvWithRegionRetry :: RetryPolicyM IO -> Region -> IO Env
discoverAWSEnvWithRegionRetry rpol r = do
  d <- getDebugging
  e <- recovering rpol [(\_ -> Handler catchAuthError)] $ \_ -> newMismiEnv r Discover
  pure $ setDebugging d e


newMismiEnv :: (Applicative m, MonadIO m, MonadCatch m) => Region -> Credentials -> m Env
newMismiEnv r c = do
  e <- newEnv c
  pure $ e & envRegion .~ r

catchAuthError :: AuthError -> IO Bool
-- MDS sometimes has transient failures.
catchAuthError (RetrievalError _)   = pure True
-- 'MissingFileError' is rethrown from 'getAuth' in
-- 'Discover' mode if 'isEC2' (which queries the MDS) returns
-- 'False'.
-- FIXME(sio): fix this upstream so we can distinguish between
-- legit 'MissingFileError's and MDS failures.
catchAuthError (MissingFileError _) = pure True
-- Everything else is unlikely to be transient.
catchAuthError _                    = pure False

renderRegionError :: RegionError -> Text
renderRegionError e =
  case e of
    UnknownRegion r ->
      "Unknown region: " <> r
    MissingRegion ->
      "Environment variable AWS_DEFAULT_REGION was not found"
