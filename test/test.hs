import           Control.Monad

import qualified Test.Mismi.S3.Commands
import qualified Test.Mismi.S3.Data
import qualified Test.Mismi.SQS.Commands

import           System.Exit
import           System.IO

main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      Test.Mismi.S3.Commands.tests
    , Test.Mismi.S3.Data.tests
    , Test.Mismi.SQS.Commands.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
