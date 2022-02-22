module HaskWarrior.DB where

import ClassyPrelude
import Control.Monad.Logger
import Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Ok
import HaskWarrior.Common as C
import HaskWarrior.PrettyPrint
import Lib as P
import Text.Read

instance FromRow TaskRecord where
  fromRow = TaskRecord <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromField TaskStatus where
  fromField f = case fieldData f of
    SQLText t -> Ok ((read . T.unpack) t)
    _ -> Errors []

class (Monad m) => TaskRepo m where
  listTasks :: m [TaskRecord]
  addTask :: Task -> m ()
  deleteTask :: TaskId -> m ()
  initDb :: Text -> m ()

instance TaskRepo Hask where
  addTask task = liftIO $ do
    conn <- open "test.db"
    currentTime <- getCurrentTime
    execute conn "INSERT INTO task (name, description, created, modified, due, status) VALUES (?,?,?,?,?,?)" (C.title task, C.description task, currentTime, currentTime, currentTime, show Ready)
    close conn

  listTasks = liftIO $ do
    conn <- open "test.db"
    r <- query_ conn "SELECT * FROM task" :: IO [TaskRecord]
    close conn
    return r

  deleteTask id = liftIO $ do
    conn <- open "test.db"
    r <- execute conn "DELETE FROM task WHERE id = ?" (Only id)
    close conn
    return r

  initDb file = liftIO $ do
    conn <- open (T.unpack file)
    execute_
      conn
      "CREATE TABLE IF NOT EXISTS task (\
      \id INTEGER         PRIMARY KEY, \
      \name TEXT          NOT NULL, \
      \description TEXT, \
      \created DATE       NOT NULL, \
      \modified DATE      NOT NULL, \
      \due DATE, \
      \status TEXT        NOT NULL)"
    close conn
