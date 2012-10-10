module PostTest where

import Common
import PostInstance
import Database.PostgreSQL.Simple
import Control.Monad.Trans.Error
import System.Environment
import Safe
import Control.Monad.IO.Class

main = do
  args <- getArgs
  ret <- runErrorT $ do
    c <- execLeft $ connect $ ConnectInfo "127.0.0.1" 5432 "test" "test" "test"
    argsExec args c
  case ret of
    Left e -> putStrLn e
    Right _ -> return ()