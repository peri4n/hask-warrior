module Main where

import Options.Applicative
import Lib

type TaskId = Int

data Command
  = Insert TaskId 
  | Delete TaskId

main :: IO ()
main = run =<< execParser
    (parseCommand `withInfo` "Interactive Task Manager")

parseCommand :: Parser Command
parseCommand = subparser $
  command "insert" (parseInsert `withInfo` "Create a task") <>
  command "delete" (parseInsert `withInfo` "Delete a task")

parseInsert :: Parser Command
parseInsert = Insert <$> argument auto (metavar "TASKID")

parseDelete :: Parser Command
parseDelete = Delete <$> argument auto (metavar "TASKID")

run :: Command -> IO ()
run (Insert id) = putStrLn $ "Inserting Task " ++ (show id)
run (Delete id) = putStrLn $ "Deleting Task " ++ (show id)

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
