{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Mismi.SQS.Commands (
    onQueue
  , createQueue
  , createQueueRaw
  , deleteQueue
  , readMessages
  , writeMessage
  , deleteMessage
  ) where

import           Control.Lens ((^.), (.~))
import           Control.Exception.Lens
import           Control.Monad.Catch

import qualified Data.List as DL
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M

import           Mismi
import           Mismi.Amazonka
import           Mismi.SQS.Amazonka as A hiding (createQueue, deleteQueue, deleteMessage)
import qualified Mismi.SQS.Amazonka as A
import           Mismi.SQS.Data

import           P


-- | Create a queue, which may be in a different region than our global/current one (which will be ignored)
onQueue :: Queue -> Maybe Int -> (QueueUrl -> AWS a) -> AWS a
onQueue (Queue q r) v action =
  within (fromMismiRegion r) (action =<< createQueue q v)

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_CreateQueue.html
createQueueRaw :: QueueName -> Maybe Int -> AWS QueueUrl
createQueueRaw q v = do
  res <- handleExists . send $ A.createQueue (renderQueueName q) &
           cqAttributes .~
             (M.fromList . maybeToList
                $ ((QANVisibilityTimeout,) <$> ((T.pack . show) <$> v)))
  maybe
    (throwM . Invariant $ "Failed to create new queue: " <> (T.pack . show) q)
    (pure . QueueUrl)
    (res ^. cqrsQueueURL)
  where
    -- If queue already exists (and has different VisibilityTimeout)
    handleExists = handling _QueueNameExists $ \_ ->
      -- Get existing queue (using default parameters)
      send $ A.createQueue (renderQueueName q)

-- | Returns the QueueUrl if the Queue already exists and if it doesn't.
-- calls `createQueueRaw` to create the Queue.
createQueue :: QueueName -> Maybe Int -> AWS QueueUrl
createQueue q v = do
  res <- send $ listQueues & lqQueueNamePrefix .~ Just (renderQueueName q)
  maybe
    (createQueueRaw q v)
    (pure . QueueUrl)
    (listToMaybe . filter isMatchingQueueName $ res ^. lqrsQueueURLs)
  where
    isMatchingQueueName url =
      case T.split (== '/') url of
        [] -> False
        xs -> if DL.last xs == renderQueueName q
                then True
                else False



-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_DeleteQueue.html
deleteQueue :: QueueUrl -> AWS ()
deleteQueue =
  void . send . A.deleteQueue . renderQueueUrl

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html
writeMessage :: QueueUrl -> Text -> Maybe Int -> AWS (MessageId)
writeMessage q m d = do
  res <- send $ A.sendMessage (renderQueueUrl q) m & smDelaySeconds .~ d
  maybe
    (throwM . Invariant $ "Failed to parse MessageId")
    (pure . MessageId)
    (res ^. smrsMessageId)

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_ReceiveMessage.html
readMessages :: QueueUrl -> Maybe Int -> Maybe Int -> AWS [A.Message]
readMessages q n w = do
  res <- send $ A.receiveMessage (renderQueueUrl q) &
           rmMaxNumberOfMessages .~ n &
           rmWaitTimeSeconds .~ w

  pure $ res ^. rmrsMessages

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_DeleteMessage.html
deleteMessage :: QueueUrl -> A.Message -> AWS ()
deleteMessage q m = do
   i <- maybe (throwM . Invariant $ "MessageId cannot be Nothing") pure (m ^. mReceiptHandle)
   void . send $ A.deleteMessage (renderQueueUrl q) i
