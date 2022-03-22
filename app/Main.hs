{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Options.Applicative
import ClassyPrelude
import HaskWarrior.CLI
import Lib
import Control.Monad.Logger
import Control.Monad.Reader
import HaskWarrior.Common
import Conferer

hask :: Hask ()
hask = do
  config <- ask
  logDebugN $ "Writing to " <> appConfigDbFile config
  app config
  return ()

main :: IO ()
main = do
  config <- getConfig
  runStdoutLoggingT $ runReaderT (runHask hask) config

getConfig :: IO AppConfig
getConfig = do
   config <- mkConfig "hask"
   fetch config
