import           Control.Monad (unless)

import           System.Exit (exitFailure)
import           System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Test.IO.Mismi.SQS.Commands
import qualified Test.Mismi as Mismi

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence =<< Mismi.enableTests "AWS_TEST_SQS" [] [
      Test.IO.Mismi.SQS.Commands.tests
    ]

  unless (and results) exitFailure
