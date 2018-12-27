{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.Amazonka (
    sendMultipart
  , newMultipart
  ) where

import           Control.Monad.Catch
import           Control.Monad.Reader (ask)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Except (runExceptT)

import           Data.Text as T
import           Data.Text.Encoding (encodeUtf8)

import           Hedgehog

import           Mismi.S3
import qualified Mismi.S3.Amazonka as A
import           Mismi.S3.Internal

import           P

import           Test.Mismi.S3


sendMultipart :: Text -> Address -> Int -> Text -> AWS ()
sendMultipart t a i ui = do
  let req = f' A.uploadPart a i ui (A.toBody $ encodeUtf8 t)
  void $ A.send req

newMultipart :: PropertyT AWS (Address, Text)
newMultipart = do
  a <- newAddress
  result <- lift $ do
    r <- createMultipartUpload a
    e <- ask
    void $ register (either throwM pure =<< runExceptT (runAWS e $ abortMultipart' a r))
    void $ register (either throwM pure =<< runExceptT (runAWS e $ listRecursively a >>= mapM_ delete >> delete a))
    pure (a, r)
  pure result
