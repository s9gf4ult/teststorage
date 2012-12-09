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
  c <-  connect $ ConnectInfo "127.0.0.1" 5432 "test" "test" "test"
  ret <- argsExec args c
  return ()