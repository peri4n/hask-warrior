{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import ClassyPrelude
import Control.Monad.IO.Class
import Control.Monad.Logger
import Lib
import Options.Applicative

type TaskId = Int

data Command
  = Add TaskId
  | Delete TaskId

main :: IO ()
main = app =<< execParser
    (parseCommand `withInfo` "Interactive Task Manager")

parseCommand :: Parser Command
parseCommand = subparser $
  command "add" (parseAdd `withInfo` "Create a task") <>
  command "delete" (parseDelete `withInfo` "Delete a task")

parseAdd :: Parser Command
parseAdd = Add <$> argument auto (metavar "TASKID")

parseDelete :: Parser Command
parseDelete = Delete <$> argument auto (metavar "TASKID")

app :: Command -> IO ()
app (Add id) = runStdoutLoggingT (runHask $ addTask id)
app (Delete id) = runStdoutLoggingT (runHask $ deleteTask id)

addTask :: TaskId -> Hask ()
addTask id = do
  logInfoN $ "Adding task with id " <> (tshow id)
  liftIO $ putStrLn "Write to DB"

deleteTask :: TaskId -> Hask ()
deleteTask id = do
  logInfoN $ "Deleting task with id " <> (tshow id)
  liftIO $ putStrLn "Remove from DB"

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

newtype Hask a = Hask { runHask :: LoggingT IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadLogger)
