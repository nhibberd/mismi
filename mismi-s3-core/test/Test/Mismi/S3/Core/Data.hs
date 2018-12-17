{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.S3.Core.Data (tests) where

import qualified Data.Text as T
import qualified Data.List as L

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Mismi.S3.Core.Data

import           P

import           Test.Mismi.S3.Core.Gen


prop_append :: Property
prop_append =
  property $ do
    p1 <- forAll genKey
    p2 <- forAll genKey
    T.count "//" (unKey (p1 // p2)) === 0


prop_appendEdge :: Property
prop_appendEdge =
  property $ do
    m <- forAll $ Gen.text (Range.singleton 5) Gen.alphaNum
    s <- forAll $ Gen.text (Range.singleton 5) Gen.alphaNum
    Key (m <> "/") // Key s === Key (m <> "/" <> s)
    Key m // Key ("/" <> s) === Key (m <> "/" <> s)
    Key m // Key s === Key (m <> "/" <> s)


prop_parse :: Property
prop_parse =
  property $ do
    a <- forAll genAddress
    addressFromText (addressToText a) === Just a

prop_parse_bucket :: Property
prop_parse_bucket =
  property $ do
    b <- forAll genBucket
    addressFromText ("s3://" <> unBucket b) === Just (Address b (Key ""))

prop_sorted :: Property
prop_sorted =
  withTests 10 . property $ do
    addresses <- forAll $ Gen.list (Range.linear 0 100) genAddress
    fmap addressToText (L.sort addresses) === L.sort (fmap addressToText addresses)

prop_withKey :: Property
prop_withKey =
  property $ do
    a <- forAll genAddress
    withKey id a === a

prop_withKey_dirname :: Property
prop_withKey_dirname =
  property $ do
    a <- forAll genAddress
    key (withKey dirname a) === (dirname . key) a

prop_withKey_key :: Property
prop_withKey_key =
  property $ do
    a <- forAll genAddress
    k <- forAll genKey
    key (withKey (// k) a) === (key a) // k

prop_basename :: Property
prop_basename =
  property $ do
    k <- forAll genKey
    bn <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    basename (k // (Key bn)) === Just bn

prop_basename_prefix :: Property
prop_basename_prefix =
  property $ do
    k <- forAll genKey
    bn <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    basename (k // (Key $ bn <> "/")) === Nothing

prop_basename_root :: Property
prop_basename_root =
  property $
    basename (Key "") === Nothing

prop_dirname :: Property
prop_dirname =
  property $ do
    a <- forAll genAddress
    t <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let
      k = Key t
    dirname (key a // k) === Key (T.dropWhileEnd ('/' ==) . unKey . key $ a)

prop_commonPrefix :: Property
prop_commonPrefix =
  property $ do
    a <- forAll genAddress
    k <- forAll genKey
    removeCommonPrefix a (withKey (// k) a) === Just k

prop_commonPrefix_fail :: Property
prop_commonPrefix_fail =
  property $ do
    a <- forAll genAddress
    k <- forAll genKey
    removeCommonPrefix (withKey (// k) a) a === Nothing


tests :: IO Bool
tests =
  checkSequential $$(discover)
