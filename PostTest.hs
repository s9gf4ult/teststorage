module PostTest where

import Common
import PostInstance
import Database.PostgreSQL.Simple
import Control.Monad.Trans.Error
import System.Environment
import Safe
import Control.Monad.IO.Class

main = do
  ret <- runErrorT $ do
    a <- liftIO getArgs
    c <- execLeft $ connect $ ConnectInfo "127.0.0.1" 5432 "test" "test" "test"
    case a of
      [ra] -> do
        aa <- maybeToErrorT "pleas insert the number of things to thest with" $ readMay ra
        work aa c
  
      _ -> work 10000 c
  
  case ret of
    Left e -> putStrLn e
    Right _ -> return ()
  where
    work i a = do
      littleTest i a
      copyTest i a
      aggTest i a
  