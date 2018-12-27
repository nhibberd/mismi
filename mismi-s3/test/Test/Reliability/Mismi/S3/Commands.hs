{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Reliability.Mismi.S3.Commands where

import           Control.Monad (replicateM_)
import           Control.Monad.Catch (throwM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (runExceptT)

import           Data.Either (isRight)
import qualified Data.Text.IO as T

import           Hedgehog
import           Hedgehog.Internal.Property (TestLimit (..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Mismi.S3

import           P

import           System.FilePath ((</>))
import           System.IO.Error (userError)
import qualified System.IO.Unsafe as Unsafe

import           Test.Mismi.S3
import qualified Test.Reliability.Reliability as Reliability

testSize :: TestLimit
testSize =
  TestLimit $
    Unsafe.unsafePerformIO Reliability.getMaxSuccess

prop_sync :: Property
prop_sync =
  withTests testSize . property . liftAWS $ do
    m <- forAll $ Gen.text (Range.linear 10 20) Gen.alphaNum
    a <- newAddress
    b <- newAddress
    i <- Reliability.testSize
    createSmallFiles a m i
    r <- lift . runExceptT $ syncWithMode OverwriteSync a b 10
    lift . forM_ (files a m i) $ \e ->
      exists e >>= \e' ->
        when (e' == False) (throwM $ userError "Output files do not exist")
    assert $ isRight r

prop_list :: Property
prop_list =
  withTests testSize . property . liftAWS $ do
    m <- forAll $ Gen.text (Range.linear 10 20) Gen.alphaNum
    a <- newAddress
    i <- Reliability.testSize
    createSmallFiles a m i
    lift $ replicateM_ 100 (list a >>= \z -> when (length z /=  i) (throwM $ userError "List is not the same as original response"))


prop_upload_single :: Property
prop_upload_single =
  withTests testSize . property . liftAWS $ do
    m <- forAll $ Gen.text (Range.linear 10 20) Gen.alphaNum
    a <- newAddress
    i <- Reliability.testSize
    p <- newFilePath
    l <- forAll $ genLocalPath
    let f = p </> localPath l
    liftIO $ T.writeFile f "data"
    lift . mapM_ (uploadOrFail f) $ files a m i


tests :: IO Bool
tests =
  checkSequential $$(discover)
