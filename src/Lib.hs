module Lib where

import HaskWarrior.Common
import ClassyPrelude

data TaskRecord = TaskRecord 
    { id :: TaskId
    , title :: String
    , description :: String
    , created :: UTCTime
    , modified :: UTCTime 
    , due :: UTCTime 
    , status :: TaskStatus
    } deriving Show

