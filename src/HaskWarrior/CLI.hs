module HaskWarrior.CLI (app, run, parseCommand, Command(..)) where

import ClassyPrelude
import Control.Monad (void)
import HaskWarrior.Common
import HaskWarrior.DB
import Options.Applicative

data Command
  = AddTask TaskName String
  | InitDb (Maybe Text)
  | DeleteTask TaskId
  | ListTasks

parseCommand :: Parser Command
parseCommand =
  subparser $
    command "add" (parseAdd `withInfo` "Creates a task")
      <> command "init" (parseInit `withInfo` "Initializes task database")
      <> command "delete" (parseDelete `withInfo` "Deletes a task")
      <> command "list" (parseList `withInfo` "Lists all tasks")

parseAdd :: Parser Command
parseAdd =
  AddTask
    <$> argument str (metavar "TASKNAME")
    <*> strOption
      ( short 'd'
          <> long "desc"
          <> metavar "DESCRIPTION"
          <> help "Task description"
          <> value ""
      )

parseInit :: Parser Command
parseInit = InitDb <$> option auto (value (Just "test.db") <> metavar "DB_PATH")

parseList :: Parser Command
parseList = pure ListTasks

parseDelete :: Parser Command
parseDelete = DeleteTask <$> argument auto (metavar "TASKID")

run :: (TaskRepo m) => Env -> Command -> m ()
run _ (AddTask name description) = addTask $ mkTask name description
run env (InitDb db) = initDb (fromMaybe (dbFile env) db)
run _ (DeleteTask id) = deleteTask id
run _ ListTasks = void listTasks

app :: Env -> Hask ()
app env = run env =<< liftIO (execParser (parseCommand `withInfo` "Interactive Task Manager"))

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
