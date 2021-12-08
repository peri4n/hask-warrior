module Main where

import Options.Applicative
import ClassyPrelude
import HaskWarrior.Common
import HaskWarrior.CLI
import Lib
import Control.Monad.Logger
import Control.Monad.Reader

hask :: Hask ()
hask = do
  env@(Env dbFile) <- ask
  logDebugN $ "Writing to " <> dbFile
  app env
  return ()

main :: IO ()
main =
    let ctx = Env "test.db"
    in  runStdoutLoggingT $ runReaderT (runHask hask) ctx
