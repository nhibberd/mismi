{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Mismi.SQS.Gen (
    NonEmptyMessage (..)
  , genNonEmptyMessage

  , genQueue
  , genQueueName
  , genSQSRegion
  ) where

import qualified Data.Text as T

import           Mismi.Kernel.Data
import           Mismi.SQS.Data

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P


newtype NonEmptyMessage =
  NonEmptyMessage {
      unMessage :: Text
    } deriving (Eq, Show)


-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html
-- invalid unicode values #x9 | #xA | #xD | [#x20 to #xD7FF] | [#xE000 to #xFFFD] | [#x10000 to #x10FFFF]
genNonEmptyMessage :: Gen NonEmptyMessage
genNonEmptyMessage =
  NonEmptyMessage <$> genSQSText

genSQSText :: Gen Text
genSQSText =
  Gen.text (Range.linear 5 100) Gen.alphaNum

genQueue :: Gen Queue
genQueue =
  Queue <$> genQueueName <*> genSQSRegion

genQueueName :: Gen QueueName
genQueueName =
  fmap (QueueName . T.pack) . Gen.list (Range.singleton 80) . Gen.element
    $ ['a'..'z'] <> ['0'..'9'] <> "-_"

genSQSRegion :: Gen MismiRegion
genSQSRegion =
  Gen.element [
      IrelandRegion
    , TokyoRegion
    , SingaporeRegion
    , SydneyRegion
    , NorthCaliforniaRegion
    , OregonRegion
    , NorthVirginiaRegion
    ]
