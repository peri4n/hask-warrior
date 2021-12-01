{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Database.SQLite.Simple
import ClassyPrelude hiding ((<>))
import Control.Monad.Logger
import Text.PrettyPrint.Boxes (text, printBox, emptyBox, (<+>), Box, (//), alignHoriz, left, right)
import Data.List (foldl1')

newtype Hask a = Hask { runHask :: LoggingT IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadLogger)

type TaskId = Int
type TaskName = Text

data TaskRecord = TaskRecord
  TaskId
  Text
  Text
  UTCTime
  UTCTime
  deriving Show

instance FromRow TaskRecord where
  fromRow = TaskRecord <$> field <*> field <*> field <*> field <*> field

listTask :: Hask ()
listTask = do
  tasks <- listTaskRecords
  liftIO $ printBox $ foldl1' (//) $ map taskToBox tasks

taskToBox :: TaskRecord -> Box
taskToBox (TaskRecord id name description due modified) = (alignHoriz left 4 . text . show) id <+> (alignHoriz left 20 . text . show) name <+> (text . show) description <+> (text . show) due <+> (text . show) modified

listTaskRecords :: Hask [TaskRecord]
listTaskRecords = liftIO $ do
  conn <- open "test.db"
  r <- query_ conn "SELECT * FROM task" :: IO [TaskRecord]
  close conn
  return r

addTask :: TaskName -> Hask ()
addTask name = do
  logInfoN $ "Adding task with id " ++ tshow name
  addTaskRecord name

deleteTask :: TaskId -> Hask ()
deleteTask id = do
  logInfoN $ "Deleting task with id " ++ tshow id
  liftIO $ putStrLn "Remove from DB"

addTaskRecord :: TaskName -> Hask ()
addTaskRecord name = liftIO $ do
  conn <- open "test.db"
  currentTime <- getCurrentTime
  execute conn "INSERT INTO task (name, description, due, modified) VALUES (?,?,?,?)" (name, "" :: String, currentTime, currentTime)
  close conn

initTask :: Hask ()
initTask = liftIO $ do
  conn <- open "test.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS task (id INTEGER PRIMARY KEY, name TEXT, description TEXT, due DATE, modified DATE)"
  close conn


