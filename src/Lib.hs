module Lib where

import HaskWarrior.Common
import ClassyPrelude

data TaskRecord = TaskRecord 
    { id :: TaskId
    , title :: Text
    , description :: Text
    , created :: UTCTime
    , modified :: UTCTime 
    , status :: TaskStatus
    } deriving Show

