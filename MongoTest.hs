module MongoTest where

import MongoInstances
import Common
import System.Environment
import Safe
import Database.MongoDB
import Control.Monad.Trans.Error

main = do
  arg <- getArgs
  p <- runIOE $ connect $ host "127.0.0.1"
  argsExec arg p
    