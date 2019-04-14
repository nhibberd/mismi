{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Mismi.SQS.Error (
    SQSError(..)
  , sqsErrorRender
  ) where

import           Control.Exception.Base (Exception)

import qualified Data.Text as T
import           Data.Typeable (Typeable)

import           P

data SQSError =
    Invariant Text
    deriving (Typeable)

instance Exception SQSError

instance Show SQSError where
  show = T.unpack . sqsErrorRender

sqsErrorRender :: SQSError -> Text
sqsErrorRender (Invariant e) =
  "[Mismi internal error] - " <> e
