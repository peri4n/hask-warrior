module Main where

import ClassyPrelude
import HaskWarrior.Common
import HaskWarrior.CLI
import Lib
import Control.Monad.Logger

main :: IO ()
main = runStdoutLoggingT $ runHask app
