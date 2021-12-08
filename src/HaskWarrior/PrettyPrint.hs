module HaskWarrior.PrettyPrint where

import ClassyPrelude
import Data.List as L
import Text.PrettyPrint.Boxes
import Lib

taskToBox :: TaskRecord -> Box
taskToBox (TaskRecord id name description due modified status) =
  (alignHoriz left 4 . text . show) id
    <+> (alignHoriz left 20 . text . show) name
    <+> (text . show) description
    <+> (text . show) due
    <+> (text . show) modified
    <+> (text . show) status

printTasks :: [TaskRecord] -> IO ()
printTasks tasks = liftIO $ printBox $ L.foldl1' (//) $ L.map taskToBox tasks
