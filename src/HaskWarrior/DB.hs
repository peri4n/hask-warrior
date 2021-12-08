module HaskWarrior.DB where

import Data.Text as T
import Text.Read
import Control.Monad.Logger
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import ClassyPrelude
import HaskWarrior.Common as C
import HaskWarrior.PrettyPrint
import Database.SQLite.Simple.Ok
import Lib as P

instance FromRow TaskRecord where
  fromRow = TaskRecord <$> field <*> field <*> field <*> field <*> field <*> field

instance FromField TaskStatus where
  fromField = \f -> case fieldData f of 
                      SQLText t -> Ok ((read . T.unpack) t)
                      _ -> Errors []

listTask :: Hask ()
listTask = do
  tasks <- listTaskRecords
  liftIO $ printTasks tasks

listTaskRecords :: Hask [TaskRecord]
listTaskRecords = liftIO $ do
  conn <- open "test.db"
  r <- query_ conn "SELECT * FROM task" :: IO [TaskRecord]
  close conn
  return r

addTask :: TaskName -> Hask ()
addTask task = do
  logInfoN $ "Adding task with id " ++ tshow task
  task <- liftIO $ mkTask task
  addTaskRecord task

deleteTask :: TaskId -> Hask ()
deleteTask id = do
  logInfoN $ "Deleting task with id " ++ tshow id
  liftIO $ putStrLn "Remove from DB"

addTaskRecord :: Task -> Hask ()
addTaskRecord task = liftIO $ do
  conn <- open "test.db"
  currentTime <- getCurrentTime
  execute conn "INSERT INTO task (name, description, due, modified, status) VALUES (?,?,?,?,?)" (C.title task, "" :: String, currentTime, currentTime, show Completed)
  close conn

initTask :: Text -> Hask ()
initTask file = liftIO $ do
  conn <- open (T.unpack file)
  execute_ conn "CREATE TABLE IF NOT EXISTS task (id INTEGER PRIMARY KEY, name TEXT, description TEXT, due DATE, modified DATE, status TEXT)"
  close conn


