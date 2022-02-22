{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import ClassyPrelude
import Control.Monad.State.Lazy
import Data.Text as T hiding (length)
import HaskWarrior.CLI
import HaskWarrior.Common
import HaskWarrior.DB
import Test.Hspec

newtype TestApp a = TestApp {unTestApp :: State [Task] a} deriving (Functor, Applicative, Monad)

instance TaskRepo TestApp where
  listTasks = undefined

  addTask task = TestApp $ state (\tasks -> ((), task : tasks))

  deleteTask = undefined

  initDb = undefined

main = do
  runSpec

runSpec :: IO ()
runSpec = hspec $
  describe "run" $ do
    it "correctly adds a task" $ do
      let app = run (Env "test.db") (AddTask (T.pack "name") "foo")
      length (execState (unTestApp app) []) `shouldBe` 1
