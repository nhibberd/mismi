import           Control.Monad (unless)

import           System.Exit (exitFailure)
import           System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Test.Mismi.Kernel.Data as Data

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [
      Data.tests
    ]

  unless (and results) exitFailure
