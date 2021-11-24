{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Database.SQLite.Simple
import ClassyPrelude
import Control.Monad.IO.Class
import Control.Monad.Logger
import Lib
import Options.Applicative

type TaskId = Int

data Command
  = Add TaskId
  | Init
  | Delete TaskId
  | List

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
parseAdd = Add <$> argument auto (metavar "TASKID")

parseInit :: Parser Command
parseInit = pure Init

parseList :: Parser Command
parseList = pure List

parseDelete :: Parser Command
parseDelete = Delete <$> argument auto (metavar "TASKID")

app :: Command -> Hask ()
app (Add id) = addTask id
app Init = initTask
app (Delete id) = deleteTask id
app List = error "NotYetImplemented"

addTask :: TaskId -> Hask ()
addTask id = do
  logInfoN $ "Adding task with id " <> tshow id
  liftIO $ putStrLn "Write to DB"

deleteTask :: TaskId -> Hask ()
deleteTask id = do
  logInfoN $ "Deleting task with id " <> tshow id
  liftIO $ putStrLn "Remove from DB"

initTask :: Hask ()
initTask = liftIO $ do
  conn <- open "test.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, name TEXT, description TEXT, due DATE, modified DATE)"
  close conn

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

newtype Hask a = Hask { runHask :: LoggingT IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadLogger)
