import           Control.Monad (unless)

import           System.Exit (exitFailure)
import           System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Test.Mismi.S3.Commands
import qualified Test.Mismi.S3.Internal

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [
      Test.Mismi.S3.Commands.tests
    , Test.Mismi.S3.Internal.tests
    ]

  unless (and results) exitFailure
