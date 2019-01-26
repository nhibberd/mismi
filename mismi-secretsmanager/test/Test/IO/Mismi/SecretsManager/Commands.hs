{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.IO.Mismi.SecretsManager.Commands (
    tests
  ) where

import           Control.Monad.Trans.Class (lift)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Mismi.SecretsManager.Data
import qualified Mismi.SecretsManager.Commands as Secrets

import           P

import           Test.Mismi

genToken :: Gen SecretToken
genToken =
  SecretToken <$> Gen.text (Range.linear 32 64) Gen.alphaNum

genString :: Gen SecretString
genString =
  SecretString <$> Gen.text (Range.linear 1 10) Gen.alphaNum

prop_missing_string :: Property
prop_missing_string =
  withTests 1 . property . liftAWS $ do
    let
      path = SecretPath "fred-test-missing"
    r1 <- lift $ Secrets.readString path
    r1 === Nothing

prop_write_missing_string :: Property
prop_write_missing_string =
  withTests 1 . property . liftAWS $ do
    let
      path = SecretPath "fred-test-missing"
    token <- forAll genToken
    string <- forAll genString
    r1 <- lift $ Secrets.writeString path token string
    r1 === WriteStringMissing

prop_create_string :: Property
prop_create_string =
  withShrinks 1 . withTests 1 . property . liftAWS $ do
    let
      path = SecretPath "fred-test"
    token <- forAll genToken
    stringOne <- forAll genString
    stringTwo <- forAll genString

    _ <- lift $ Secrets.createString path token stringOne
    r <- lift $ Secrets.createString path token stringOne
    r === CreateStringAlreadyExists

    _ <- lift $ Secrets.writeString path token stringTwo

    string <- lift $ Secrets.readString path
    string === Just stringTwo

tests :: IO Bool
tests =
  checkParallel $$(discover)
