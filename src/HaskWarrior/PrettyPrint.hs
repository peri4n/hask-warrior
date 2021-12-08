module HaskWarrior.PrettyPrint where

import Data.List as L
import ClassyPrelude
import Text.PrettyPrint.Boxes 
import HaskWarrior.Common

taskToBox :: TaskRecord -> Box
taskToBox (TaskRecord id name description due modified) = (alignHoriz left 4 . text . show) id <+> (alignHoriz left 20 . text . show) name <+> (text . show) description <+> (text . show) due <+> (text . show) modified

printTasks :: [TaskRecord] -> Hask ()
printTasks tasks = liftIO $ printBox $ L.foldl1' (//) $ L.map taskToBox tasks

