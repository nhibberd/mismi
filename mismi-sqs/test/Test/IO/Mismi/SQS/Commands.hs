{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.IO.Mismi.SQS.Commands (
    tests
  ) where

import           Control.Lens ((^.))
import           Control.Monad.Trans.Class (lift)

import           Hedgehog

import           Mismi (AWS)
import qualified Mismi.SQS.Amazonka as A
import qualified Mismi.SQS.Commands as SQS
import           Mismi.SQS.Data

import           P

import qualified Test.IO.Mismi.SQS.Util as Util
import           Test.Mismi (liftAWS)
import qualified Test.Mismi.SQS.Gen as Gen

prop_write_read :: Property
prop_write_read =
  withTests 2 . property . Util.withQueueUrl $ \q -> do
    msg <- forAll Gen.genNonEmptyMessage
    writeMessage q msg
    ms <- readOneMessage q
    validateMessage msg ms

prop_write_read_twice_visible :: Property
prop_write_read_twice_visible =
  withTests 2 . property . Util.withQueueUrlVisibility (Visibility 0) $ \q -> do
    msg <- forAll Gen.genNonEmptyMessage
    writeMessage q msg
    ms1 <- readOneMessage q
    validateMessage msg ms1
    ms2 <- readOneMessage q
    validateMessage msg ms2

prop_write_read_twice_hidden :: Property
prop_write_read_twice_hidden =
  withTests 2 . property . Util.withQueueUrl $ \q -> do
    msg <- forAll Gen.genNonEmptyMessage
    writeMessage q msg
    ms1 <- readOneMessage q
    ms2 <- readOneMessage q

    validateMessage msg ms1
    [] === fmap (^. A.mBody) ms2

prop_write_delete_read :: Property
prop_write_delete_read =
  withTests 2 . property . Util.withQueueUrl $ \q -> do
    msg <- forAll Gen.genNonEmptyMessage
    writeMessage q msg
    ms <- readOneMessage q
    lift $ forM_ ms (SQS.deleteMessage q)
    res <- readOneMessage q
    [] === res

prop_with_queue :: Property
prop_with_queue  =
  withTests 2 . property . Util.withQueue $ \q -> do
    m@(Gen.NonEmptyMessage msg) <- forAll Gen.genNonEmptyMessage
    ls <- lift . SQS.onQueue q Nothing $ \url -> do
      void $ SQS.writeMessage url msg
      SQS.readMessages url (Just $ MessageCount 1) Nothing
    validateMessage m ls

prop_create_upgrade :: Property
prop_create_upgrade =
  withTests 2 . property . liftAWS $ do
    msg <- forAll Gen.genNonEmptyMessage
    queue <- forAll Gen.genQueue
    -- create queue with default VisibilityTimeout
    q <- Util.newQueueWithVisibility Nothing queue

    -- get existing queue with non-default VisibilityTimeout
    q' <- lift $ SQS.createQueue (queueName queue) (Just $ Visibility 8400)
    writeMessage q msg
    ms <- readOneMessage q'
    validateMessage msg ms

prop_create_downgrade :: Property
prop_create_downgrade =
  withTests 2 . property .
    -- create queue with non-default VisibilityTimeout
    Util.withQueueAndUrlVisibility (Visibility 8400) $ \(queue', q) -> do
      msg <- forAll Gen.genNonEmptyMessage

      -- get existing queue with default VisibilityTimeout
      q' <- lift $ SQS.createQueue (queueName queue') Nothing
      writeMessage q msg
      res <- readOneMessage q'
      validateMessage msg res

prop_create_queue :: Property
prop_create_queue =
  withTests 1 . property . Util.withQueue $ \queue -> do
    lift . SQS.onQueue queue (Just $ Visibility 8400) $ \_ ->
      pure ()
    lift . SQS.onQueue queue (Just $ Visibility 8400) $ \_ ->
      pure ()
    success

validateMessage :: Monad m => Gen.NonEmptyMessage -> [A.Message] -> PropertyT m ()
validateMessage (Gen.NonEmptyMessage b) response =
  [Just b] === fmap (^. A.mBody) response

writeMessage :: QueueUrl -> Gen.NonEmptyMessage -> PropertyT AWS ()
writeMessage q (Gen.NonEmptyMessage b) =
  void . lift $ SQS.writeMessage q b

readOneMessage :: QueueUrl -> PropertyT AWS [A.Message]
readOneMessage q =
  lift $ SQS.readMessages q oneMessage Nothing

oneMessage :: Maybe MessageCount
oneMessage =
  Just $ MessageCount 1


tests :: IO Bool
tests =
  checkParallel $$(discover)
