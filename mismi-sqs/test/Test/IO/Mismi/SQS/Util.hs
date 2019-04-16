{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
module Test.IO.Mismi.SQS.Util (
    withQueue
  , withQueueUrl
  , withQueueUrlVisibility
  , withQueueAndUrlVisibility
  , liftAWSQueue
  , newQueue
  , newQueueAWS
  , newQueueWithVisibility
  , newQueueWithVisibilityAWS
  ) where

import           Control.Monad.Reader (ask)
import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (register)

import           Hedgehog

import           Mismi (AWS, fromMismiRegion)
import qualified Mismi.Control as Control
import qualified Mismi.Amazonka as A
import           Mismi.SQS.Data (Queue (..), QueueUrl, Visibility (..))
import qualified Mismi.SQS.Commands as SQS

import           P

import qualified Test.Mismi as X
import qualified Test.Mismi.SQS.Gen as Gen

withQueue :: (Queue -> PropertyT AWS a) -> PropertyT IO a
withQueue f = do
  q <- forAll Gen.genQueue
  liftAWSQueue q $
    f q

withQueueUrl :: (QueueUrl -> PropertyT AWS a) -> PropertyT IO a
withQueueUrl f = do
  q <- forAll Gen.genQueue
  liftAWSQueue q $ do
    url <- newQueue q
    f url

withQueueUrlVisibility :: Visibility -> (QueueUrl -> PropertyT AWS a) -> PropertyT IO a
withQueueUrlVisibility v f = do
  q <- forAll Gen.genQueue
  liftAWSQueue q $ do
    url <- newQueueWithVisibility (Just v) q
    f url

withQueueAndUrlVisibility :: Visibility -> ((Queue, QueueUrl) -> PropertyT AWS a) -> PropertyT IO a
withQueueAndUrlVisibility v f = do
  q <- forAll Gen.genQueue
  liftAWSQueue q $ do
    url <- newQueueWithVisibility (Just v) q
    f (q, url)

liftAWSQueue :: Queue -> PropertyT AWS a -> PropertyT IO a
liftAWSQueue (Queue _ r) =
  hoist (X.runAWSDefaultRegion . A.within (fromMismiRegion r))

newQueue :: Queue -> PropertyT AWS QueueUrl
newQueue q =
  lift (newQueueAWS q)

newQueueAWS :: Queue -> AWS QueueUrl
newQueueAWS q =
  newQueueWithVisibilityAWS testVisibilityTimeout q

newQueueWithVisibility :: Maybe Visibility -> Queue -> PropertyT AWS QueueUrl
newQueueWithVisibility v q =
  lift (newQueueWithVisibilityAWS v q)

newQueueWithVisibilityAWS :: Maybe Visibility -> Queue -> AWS QueueUrl
newQueueWithVisibilityAWS v (Queue qn r) =
  A.within (fromMismiRegion r) $ do
    e <- ask
    url <- SQS.createQueue qn v
    void $ register (Control.unsafeRunAWS e $ SQS.deleteQueue url)
    pure url

testVisibilityTimeout :: Maybe Visibility
testVisibilityTimeout =
  Just $ Visibility 8400
