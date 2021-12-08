module HaskWarrior.CLI 
  ( app, parseCommand ) where

import HaskWarrior.Common
import HaskWarrior.DB
import Options.Applicative
import ClassyPrelude

data Command
  = Add TaskName
  | Init
  | Delete TaskId
  | List

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

run :: Env -> Command -> Hask ()
run _ (Add name) = addTask name
run env Init = initTask (dbFile env)
run _ (Delete id) = deleteTask id
run _ List = listTask

app :: Env -> Hask ()
app env = (run env) =<< liftIO (execParser (parseCommand `withInfo` "Interactive Task Manager"))

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
