{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mismi.SQS.Commands (
    onQueue
  , createQueue
  , createQueueRaw
  , deleteQueue
  , readMessages
  , readMessagesWithAttributes
  , writeMessage
  , deleteMessage
  ) where

import           Control.Lens ((^.), (.~))
import           Control.Exception.Lens (handling)
import           Control.Monad.Catch (throwM)

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M

import           Mismi (AWS, fromMismiRegion)
import           Mismi.Control (handle400Error)
import           Mismi.SQS.Amazonka as A hiding (createQueue, deleteQueue, deleteMessage)
import qualified Mismi.SQS.Amazonka as A
import           Mismi.SQS.Data
import           Mismi.SQS.Error (SQSError (..))

import           P


readMessages :: QueueUrl -> Maybe MessageCount -> Maybe WaitTime -> AWS [A.Message]
readMessages q c w = do
  res <- A.send $ A.receiveMessage (renderQueueUrl q)
    & A.rmMaxNumberOfMessages .~  fmap messageCount c
    & A.rmWaitTimeSeconds .~ fmap waitTime w
  pure $ res ^. rmrsMessages

readMessagesWithAttributes :: QueueUrl -> Maybe MessageCount -> Maybe WaitTime -> [Text] -> AWS [A.Message]
readMessagesWithAttributes q c w keys = do
  res <- A.send $ A.receiveMessage (renderQueueUrl q)
    & A.rmMaxNumberOfMessages .~ fmap messageCount c
    & A.rmWaitTimeSeconds .~ fmap waitTime w
    & A.rmMessageAttributeNames .~ keys
  pure $ res ^. rmrsMessages

-- | Create a queue, which may be in a different region than our global/current one (which will be ignored)
onQueue :: Queue -> Maybe Visibility -> (QueueUrl -> AWS a) -> AWS a
onQueue (Queue q r) v action =
  within (fromMismiRegion r) (action =<< createQueue q v)

createQueueRaw :: QueueName -> Maybe Visibility -> AWS QueueUrl
createQueueRaw q vis = do
  let
    visbility v =
      (A.QANVisibilityTimeout, T.pack . show $ visibility v)

  res <- handleExists q . A.send $ A.createQueue (renderQueueName q)
    & A.cqAttributes .~ (M.fromList . maybeToList $ fmap visbility vis)
  maybe
    (throwM . Invariant $ "Failed to create new queue: " <> (T.pack . show) q)
    (pure . QueueUrl)
    (res ^. cqrsQueueURL)

-- If queue already exists (and has different VisibilityTimeout)
handleExists :: QueueName -> AWS A.CreateQueueResponse -> AWS A.CreateQueueResponse
handleExists q =
  handling _QueueNameExists $ \_ ->
    -- Get existing queue (using default parameters)
    send $ A.createQueue (renderQueueName q)

-- | Returns the QueueUrl if the Queue already exists and if it doesn't.
-- calls `createQueueRaw` to create the Queue.
createQueue :: QueueName -> Maybe Visibility -> AWS QueueUrl
createQueue q v = do
  let
    handler =
      handle400Error "AWS.SimpleQueueService.NonExistentQueue"

  res <- handler . A.send $
    listQueues
      & lqQueueNamePrefix .~ Just (renderQueueName q)

  case res of
    Nothing ->
      createQueue q v

    Just res' ->
      maybe
        (createQueueRaw q v)
        (pure . QueueUrl)
        (listToMaybe . List.filter (isMatchingQueueName q) $ res' ^. lqrsQueueURLs)

isMatchingQueueName :: QueueName -> Text -> Bool
isMatchingQueueName q url =
  case T.split (== '/') url of
    [] ->
      False
    xs ->
      List.last xs == renderQueueName q

deleteQueue :: QueueUrl -> AWS ()
deleteQueue =
  void . send . A.deleteQueue . renderQueueUrl

writeMessage :: QueueUrl -> Text -> AWS (MessageId)
writeMessage q m = do
  res <- send $ A.sendMessage (renderQueueUrl q) m
  maybe
    (throwM . Invariant $ "Failed to parse MessageId")
    (pure . MessageId)
    (res ^. smrsMessageId)

deleteMessage :: QueueUrl -> A.Message -> AWS ()
deleteMessage q m = do
   i <- maybe (throwM . Invariant $ "MessageId cannot be Nothing") pure (m ^. mReceiptHandle)
   void . send $ A.deleteMessage (renderQueueUrl q) i
