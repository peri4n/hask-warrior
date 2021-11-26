{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Database.SQLite.Simple
import ClassyPrelude
import Control.Monad.IO.Class
import Control.Monad.Logger
import Lib
import Options.Applicative

type TaskId = Int
type TaskName = Text

data Command
  = Add TaskName
  | Init
  | Delete TaskId
  | List

data TaskRecord = TaskRecord
  TaskId
  Text
  Text
  UTCTime
  UTCTime
  deriving Show

instance FromRow TaskRecord where
  fromRow = TaskRecord <$> field <*> field <*> field <*> field <*> field

main :: IO ()
main = runStdoutLoggingT . runHask . app =<< execParser
    (parseCommand `withInfo` "Interactive Task Manager")

parseCommand :: Parser Command
parseCommand = subparser $
  command "add" (parseAdd `withInfo` "Create a task") <>
  command "init" (parseInit `withInfo` "Initialize task database") <>
  command "delete" (parseDelete `withInfo` "Delete a task") <>
  command "list" (parseList `withInfo` "List all tasks")

parseAdd :: Parser Command
parseAdd = Add <$> argument str (metavar "TASKNAME")

parseInit :: Parser Command
parseInit = pure Init

parseList :: Parser Command
parseList = pure List

parseDelete :: Parser Command
parseDelete = Delete <$> argument auto (metavar "TASKID")

app :: Command -> Hask ()
app (Add name) = addTask name
app Init = initTask
app (Delete id) = deleteTask id
app List = listTask

listTask :: Hask ()
listTask = do
  tasks <- listTaskRecords
  liftIO $ mapM_ print tasks

listTaskRecords :: Hask [TaskRecord]
listTaskRecords = liftIO $ do
  conn <- open "test.db"
  r <- query_ conn "SELECT * FROM task" :: IO [TaskRecord]
  close conn
  return r

addTask :: TaskName -> Hask ()
addTask name = do
  logInfoN $ "Adding task with id " <> tshow name
  addTaskRecord name

deleteTask :: TaskId -> Hask ()
deleteTask id = do
  logInfoN $ "Deleting task with id " <> tshow id
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

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

newtype Hask a = Hask { runHask :: LoggingT IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadLogger)
