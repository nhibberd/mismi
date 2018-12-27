{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.S3 (
    module X

  , Token (..)
  , genToken

  , LocalPath (..)
  , genLocalPath

  , testBucket
  , createSmallFiles
  , files
  , newAddress
  , newAddressAWS
  , newFilePath
  , addCleanupFinalizer
  , addPrintFinalizer
  , addLocalCleanupFinalizer
  , addLocalPrintFinalizer
  ) where


import           Control.Monad.Catch
import           Control.Monad.Reader (ask)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.List as L
import           Data.Text as T
import qualified Data.Text.IO as T
import           Data.UUID as U
import           Data.UUID.V4 as U

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           System.Environment (lookupEnv)
import           System.Posix.Env
import           System.FilePath
import           System.Directory

import           Mismi.Control
import           Mismi.S3

import           P

import           Test.Mismi as X

data Token =
  Token {
      unToken :: Text
    } deriving (Eq, Show)

genToken :: Gen Token
genToken = do
  let
    genA = Gen.text (Range.linear 10 15) Gen.alphaNum
    genB = Gen.text (Range.linear 5 10) Gen.alphaNum

  n <- T.pack . show <$> (Gen.int $ Range.linear 0 10000)
  c <- genA
  m <- genB
  sep <- Gen.element ["-", "=", "."]
  pure . Token . T.intercalate sep $ [c, m, n]

data LocalPath =
  LocalPath {
      localPath :: FilePath
    } deriving (Eq, Show)

genLocalPath :: Gen LocalPath
genLocalPath =  do
  let
    gen = Gen.text (Range.linear 5 10) Gen.alphaNum

  x <- gen
  xs <- Gen.list (Range.linear 0 5) gen
  pure . LocalPath $ L.intercalate "/" (T.unpack <$> x : xs)

testBucket :: IO Bucket
testBucket =
  Bucket . T.pack . fromMaybe "ambiata-dev-view" <$> getEnv "AWS_TEST_BUCKET"

createSmallFiles :: Address -> Text -> Int -> PropertyT AWS ()
createSmallFiles prefix name n = do
  lift . mapM_ (flip write "data") $ files prefix name n

files :: Address -> Text -> Int -> [Address]
files prefix name n =
  fmap (\i -> withKey (// Key (name <> "-" <> (T.pack $ show i))) prefix) [1..n]

newAddress :: PropertyT AWS Address
newAddress =
  lift newAddressAWS

newAddressAWS :: AWS Address
newAddressAWS = do
  a <- liftIO $ do
    t <- Gen.sample genToken
    b <- testBucket
    u <- T.pack . U.toString <$> U.nextRandom
    pure $ Address b (Key . T.intercalate "/" $ ["mismi", u, unToken t])
  addCleanupFinalizer a
  addPrintFinalizer a
  pure $ a

newFilePath :: PropertyT AWS FilePath
newFilePath = do
  p <- liftIO $ do
    t <- Gen.sample genToken
    d <- getTemporaryDirectory
    u <- liftIO $ U.toString <$> U.nextRandom
    let p = d <> "/mismi/" <> u <> "-" <> (T.unpack . unToken $ t)
    createDirectoryIfMissing True p
    pure p
  lift$ addLocalCleanupFinalizer p
  lift $ addLocalPrintFinalizer p
  pure p

vk :: MonadIO m => Text -> m Bool
vk k = do
  m <- liftIO $ lookupEnv (T.unpack k)
  return $ maybe False (\v -> v == "1" || v == "true") m

addCleanupFinalizer :: Address -> AWS ()
addCleanupFinalizer a = do
  e <- ask
  r <- vk "TEST_SKIP_CLEANUP_RESOURCES"
  p <- vk "TEST_PRINT_CLEANUP_RESOURCES"
  unless r .
    void $ register (either throwM pure =<< runExceptT (runAWS e $
      listRecursively a >>= mapM_ delete >> delete a))
  when p .
    void $ register (T.putStrLn $ "Cleaning up [" <> addressToText a <> "]")

addPrintFinalizer :: Address -> AWS ()
addPrintFinalizer a = do
  r <- vk "TEST_PRINT_PATHS"
  when r .
    void $ register (T.putStrLn $ "Temporary s3 address [" <> addressToText a <> "]")

addLocalCleanupFinalizer :: FilePath -> AWS ()
addLocalCleanupFinalizer a = do
  r <- vk "TEST_SKIP_CLEANUP_RESOURCES"
  p <- vk "TEST_PRINT_CLEANUP_RESOURCES"
  unless r .
    void $ register (removeDirectoryRecursive a)
  when p .
    void $ register (T.putStrLn $ "Cleaning up [" <> T.pack a <> "]")

addLocalPrintFinalizer :: FilePath -> AWS ()
addLocalPrintFinalizer a = do
  r <- vk "TEST_PRINT_PATHS"
  when r .
    void $ register (T.putStrLn $ "Temporary local filepath [" <> T.pack a <> "]")
