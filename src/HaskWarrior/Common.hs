{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskWarrior.Common where

import ClassyPrelude
import Control.Monad.Logger

data Env = Env
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

data TaskStatus = Completed | Ready | Running deriving (Show, Read)

data Task = Task
  { title :: Text,
    description :: Text,
    created :: UTCTime,
    modified :: UTCTime,
    status :: TaskStatus
  }
  deriving (Show)

mkTask :: Text -> IO Task
mkTask title = do
  currentTime <- getCurrentTime
  return $ Task title "" currentTime currentTime Ready
