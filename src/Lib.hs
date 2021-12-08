module Lib where

import Control.Monad.Logger
import Database.SQLite.Simple
import ClassyPrelude
import HaskWarrior.Common
import HaskWarrior.PrettyPrint

instance FromRow TaskRecord where
  fromRow = TaskRecord <$> field <*> field <*> field <*> field <*> field

listTask :: Hask ()
listTask = do
  tasks <- listTaskRecords
  printTasks tasks

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


