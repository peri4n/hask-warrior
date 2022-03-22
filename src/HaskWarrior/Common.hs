{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskWarrior.Common where

import ClassyPrelude
import Conferer
import Control.Monad.Logger

data AppConfig = AppConfig
  { appConfigDbFile :: Text
  }
  deriving (Generic)

instance FromConfig AppConfig

instance DefaultConfig AppConfig where
  configDef =
    AppConfig
      { appConfigDbFile = ".config/hask/task.db"
      }

newtype Hask a = Hask {runHask :: ReaderT AppConfig (LoggingT IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader AppConfig,
      MonadLogger
    )

type TaskId = Int

type TaskName = Text

data TaskStatus = Completed | Ready | Running deriving (Show, Read, Eq)

data Task = Task
  { title :: Text,
    description :: String,
    created :: UTCTime,
    modified :: UTCTime,
    status :: TaskStatus
  }
  deriving (Show, Eq)

mkTask :: Text -> String -> Task
mkTask title desc = Task title desc currentTime currentTime Ready
  where
    currentTime = UTCTime (ModifiedJulianDay 5) 23 -- this is set to a non-sensical value to drop the side effect for now
