module HaskWarrior.PrettyPrint where

import ClassyPrelude
import Data.List as L
import Data.Time.Format
import Text.PrettyPrint.Boxes
import Lib

taskToBox :: TaskRecord -> Box
taskToBox (TaskRecord id name description created modified due status) =
  (alignHoriz right 4 . text . show) id
    <+> char '|'
    <+> (alignHoriz right 20 . text) name
    <+> char '|'
    <+> (alignHoriz right 20 . text) description
    <+> char '|'
    <+> (alignHoriz right 30 . text . (formatTime defaultTimeLocale "%c")) created
    <+> char '|'
    <+> (alignHoriz right 30 . text . (formatTime defaultTimeLocale "%c")) modified
    <+> char '|'
    <+> (alignHoriz left 10 . text . show) status

header :: Box
header = (alignHoriz right 4 . text) "ID"
    <+> char '|'
    <+> (alignHoriz right 20 . text) "Name"
    <+> char '|'
    <+> (alignHoriz right 20 . text) "Description"
    <+> char '|'
    <+> (alignHoriz right 30 . text) "Created"
    <+> char '|'
    <+> (alignHoriz right 30 . text) "Modified"
    <+> char '|'
    <+> (alignHoriz left 10 . text) "Status"

printTasks :: [TaskRecord] -> IO ()
printTasks tasks = liftIO $ printBox $ L.foldl' (//) header $ L.map taskToBox tasks
