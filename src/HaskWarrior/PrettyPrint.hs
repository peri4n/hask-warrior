module HaskWarrior.PrettyPrint where

import ClassyPrelude
import Data.List as L
import Data.Time.Format
import Text.PrettyPrint.Boxes
import Lib

taskToBox :: TaskRecord -> Box
taskToBox (TaskRecord id name description created modified due status) =
  (alignHoriz left 4 . text . show) id
    <+> char '|'
    <+> (alignHoriz right 20 . text) name
    <+> char '|'
    <+> (alignHoriz right 20 . text) description
    <+> char '|'
    <+> (text . (formatTime defaultTimeLocale "%c")) created
    <+> char '|'
    <+> (text . (formatTime defaultTimeLocale "%c")) modified
    <+> char '|'
    <+> (text . show) status

printTasks :: [TaskRecord] -> IO ()
printTasks tasks = liftIO $ printBox $ L.foldl1' (//) $ L.map taskToBox tasks
