module HaskWarrior.CLI (app, parseCommand) where

import ClassyPrelude
import HaskWarrior.Common
import HaskWarrior.DB
import Options.Applicative

data Command
  = Add TaskName String
  | Init (Maybe Text)
  | Delete TaskId
  | List

parseCommand :: Parser Command
parseCommand =
  subparser $
    command "add" (parseAdd `withInfo` "Creates a task")
      <> command "init" (parseInit `withInfo` "Initializes task database")
      <> command "delete" (parseDelete `withInfo` "Deletes a task")
      <> command "list" (parseList `withInfo` "Lists all tasks")

parseAdd :: Parser Command
parseAdd =
  Add
    <$> argument str (metavar "TASKNAME")
    <*> strOption
      ( short 'd'
          <> long "desc"
          <> metavar "DESCRIPTION"
          <> help "Task description"
          <> value ""
      )

parseInit :: Parser Command
parseInit = Init <$> option auto (value (Just "test.db") <> metavar "DB_PATH")

parseList :: Parser Command
parseList = pure List

parseDelete :: Parser Command
parseDelete = Delete <$> argument auto (metavar "TASKID")

run :: Env -> Command -> Hask ()
run _ (Add name description) = addTask name description
run env (Init db) = initTask (fromMaybe (dbFile env) db)
run _ (Delete id) = deleteTask id
run _ List = listTask

app :: Env -> Hask ()
app env = run env =<< liftIO (execParser (parseCommand `withInfo` "Interactive Task Manager"))

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
