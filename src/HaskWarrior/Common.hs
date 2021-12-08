{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskWarrior.Common where

import ClassyPrelude
import Control.Monad.Logger

newtype Hask a = Hask { runHask :: LoggingT IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadLogger)

type TaskId = Int
type TaskName = Text

data TaskRecord = TaskRecord 
    { id :: TaskId
    , title :: Text
    , description :: Text
    , created :: UTCTime
    , modified :: UTCTime 
    } deriving Show
