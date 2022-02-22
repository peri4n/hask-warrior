{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskWarrior.Common where

import ClassyPrelude
import Control.Monad.Logger

newtype Env = Env
  { dbFile :: Text
  }

newtype Hask a = Hask {runHask :: ReaderT Env (LoggingT IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Env,
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
 where currentTime = UTCTime (ModifiedJulianDay 5) 23 -- this is set to a non-sensical value to drop the side effect for now

