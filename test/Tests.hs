{-# LANGUAGE NoImplicitPrelude #-}

import           Protolude
import qualified System.IO as IO
import           System.Exit (exitFailure)

import qualified DemoTests

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering

  results <- sequence [ DemoTests.tests ]

  if (and results)
  then pass
  else exitFailure
