import           Control.Monad (unless)

import           System.Exit (exitFailure)
import           System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Test.IO.Mismi.S3.Commands
import qualified Test.IO.Mismi.S3.Control
import qualified Test.IO.Mismi.S3.Internal
import qualified Test.Mismi as Mismi

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence =<< Mismi.enableTests "AWS_TEST_S3" [
      Test.IO.Mismi.S3.Internal.tests
    ] [
      Test.IO.Mismi.S3.Internal.tests
    , Test.IO.Mismi.S3.Commands.tests
    , Test.IO.Mismi.S3.Control.tests
    ]


  unless (and results) exitFailure
