{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import ClassyPrelude
import Options.Applicative
import Lib
import Control.Monad.Logger

data Command
  = Add TaskName
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

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
