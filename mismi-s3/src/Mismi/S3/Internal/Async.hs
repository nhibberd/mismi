{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mismi.S3.Internal.Async (
    waitEitherBoth
  ) where


import           Control.Concurrent.Async (Async, waitSTM)
import           Control.Concurrent.STM (atomically, orElse, retry)

import           P

import           System.IO (IO)

waitEitherBoth :: Async a -> Async b -> Async c -> IO (Either a (b, c))
waitEitherBoth a b c =
  atomically $ do
    let
      l = waitSTM a
      r = do
        bb <- waitSTM b `orElse` (waitSTM c >> retry)
        cc <- waitSTM c
        return (bb, cc)
    fmap Left l `orElse` fmap Right r
