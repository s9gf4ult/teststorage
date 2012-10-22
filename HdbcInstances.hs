module HdbcInstances where


import Common
import Database.HDBC
import Data.Convertible
import Control.Monad.Trans
import Control.Monad.Trans.Error
import Control.Exception
import Data.Int


data HDBCPack = HDBCPack {hConn :: ConnWrapper
                         ,hInsert :: Statement
                         ,hGet :: Statement
                         ,hGetLT :: Statement
                         ,hGetGT :: Statement
                         ,hGetLGT :: Statement
                         }

toSqlVal :: Storable -> [SqlValue]
toSqlVal (Storable a b c d) = [convert a,
                               convert b,
                               convert c,
                               convert d]

fromSqlVal :: (Monad m, MonadIO m) => [SqlValue] -> ErrorT String m Storable
fromSqlVal [a, b, c, d] = do
  ac <- liftConv $ convert a
  bc <- liftConv $ convert b
  cc <- liftConv $ convert c
  dc <- liftConv $ convert d
  return $ Storable ac bc cc dc
  where
    liftConv = (mapLeftE (show :: SomeException -> String)) . ErrorT . liftIO . try . evaluate
fromSqlVal _ = throwError "wrong size of return value"

liftErr m = mapLeftE (show :: SomeException -> String) $ ErrorT $ liftIO $ try $ (m >>= evaluate)
  

instance Storage HDBCPack where
  saveS hp st = liftIO $ withWConn (hConn hp) $ \con -> withTransaction con $ \_ -> do
    executeMany (hInsert hp) $ map toSqlVal st
  
  getS hp = do
    liftErr $ execute (hGet hp) []
    vls <- liftErr $ fetchAllRows $ hGet hp
    mapM fromSqlVal vls
  
  resetS hp = liftErr $ withWConn (hConn hp) $ \con -> do
    run con "drop table if exists storables" []
    run con "create table storables (a integer, b integer, c bigint, d char(100))" []
    return ()

  getFilterA (CLT l) hp = getFilterSTMT (hGetLT hp) [convert l]
  getFilterA (CGT g) hp = getFilterSTMT (hGetGT hp) [convert g]
  getFilterA (CLGT l g) hp = getFilterSTMT (hGetLGT hp) [convert l,
                                                         convert g]

getFilterSTMT st vals = do
  liftErr $ execute st vals
  vls <- liftErr $ fetchAllRows st
  mapM fromSqlVal vls


