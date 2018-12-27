{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
module Mismi.S3.Internal.Queue (
    Queue
  , newQueue
  , readQueue
  , tryReadQueue
  , writeQueue
  , isQueueEmpty
  ) where

import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueue, tryReadTBQueue, readTBQueue, writeTBQueue, isEmptyTBQueue)

import           GHC.Conc (atomically)
#if MIN_VERSION_stm(2,5,0)
import           GHC.Natural (naturalFromInteger)
#endif

import           P

#if MIN_VERSION_stm(2,5,0)
import           Prelude (toInteger)
#endif


newtype Queue a =
  Queue {
      queue :: TBQueue a
    }

newQueue :: Int -> IO (Queue a)
newQueue i =
#if MIN_VERSION_stm(2,5,0)
  atomically $ Queue <$> newTBQueue (naturalFromInteger $ toInteger i)
#else
  atomically $ Queue <$> newTBQueue i
#endif


readQueue :: Queue a -> IO a
readQueue =
  atomically . readTBQueue . queue

tryReadQueue :: Queue a -> IO (Maybe a)
tryReadQueue =
  atomically . tryReadTBQueue . queue

writeQueue :: Queue a -> a -> IO ()
writeQueue q =
  atomically . writeTBQueue (queue q)

isQueueEmpty :: Queue a -> IO Bool
isQueueEmpty =
  atomically . isEmptyTBQueue . queue
