{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Mismi.Control (tests) where

import           Control.Monad.Catch (throwM, catchIOError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (register)

import           Data.IORef (modifyIORef, newIORef, readIORef)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Mismi.Control (awsBracket)

import           P

import           System.IO.Error (userError)

import           Test.Mismi (testAWS, liftAWS, runAWSDefaultRegion)

prop_bracket :: Property
prop_bracket =
  property $ do
    l <- forAll $ Gen.list (Range.linear 0 100) (Gen.text (Range.linear 0 50) Gen.alphaNum)
    final <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
    action <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum

    ref <- liftIO $ newIORef l
    let
      after' = (flip modifyIORef (final :))
      action' = (flip modifyIORef (action :))
    liftIO . runAWSDefaultRegion $ awsBracket (liftIO $ return ref) (liftIO . after') (liftIO . action')
    result <- liftIO $ readIORef ref
    result === final : action : l

prop_bracket_catch :: Property
prop_bracket_catch =
  property $ do
    l <- forAll $ Gen.list (Range.linear 0 100) (Gen.text (Range.linear 0 50) Gen.alphaNum)
    final <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum

    ref <- liftIO $ newIORef l
    let
      after' = (flip modifyIORef (final :))
      action' = const $ throwM (userError "")
    liftIO . runAWSDefaultRegion $
      awsBracket (liftIO $ return ref) (liftIO . after') (liftIO . action') `catchIOError` (const $ return ())
    result <- liftIO $ readIORef ref
    result === final : l


prop_testAWS :: Property
prop_testAWS =
  withTests 1 . testAWS $
    pure ()

prop_testAWS_PropertyT :: Property
prop_testAWS_PropertyT =
  withTests 1 . property . liftAWS $ do
    False === False
    True === True


prop_finalizer :: Property
prop_finalizer =
  property $ do
    ref <- liftIO $ newIORef (0 :: Int)
    liftIO . runAWSDefaultRegion $ do
      void $ register (modifyIORef ref (const $ 1))
    result <- liftIO $ readIORef ref
    result === 1

tests :: IO Bool
tests =
  checkSequential $$(discover)
