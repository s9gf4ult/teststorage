module MongoTest where

import MongoInstances
import Common
import System.Environment
import Safe
import Database.MongoDB
import Control.Monad.Trans.Error

main = do
  arg <- getArgs
  ret <- runErrorT $ do
    p <- mapLeftE show $ connect $ host "127.0.0.1"
    argsExec arg p
  case ret of
    Left e -> putStrLn e
    Right a -> return ()
    