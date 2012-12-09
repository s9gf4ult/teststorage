{-# LANGUAGE MultiParamTypeClasses #-}

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

fromSqlVal :: [SqlValue] -> Storable
fromSqlVal [a, b, c, d] = let ac = convert a
                              bc = convert b
                              cc = convert c
                              dc = convert d
                          in (Storable ac bc cc dc)
fromSqlVal _ = error "wrong size of return value"


instance Storage HDBCPack IO where
  saveS hp st = withWConn (hConn hp) $ \con -> withTransaction con $ \_ -> do
    executeMany (hInsert hp) $ map toSqlVal st
  
  getS hp = do
    execute (hGet hp) []
    vls <- fetchAllRows $ hGet hp
    return $ map fromSqlVal vls
  
  resetS hp = withWConn (hConn hp) $ \con -> do
    run con "drop table if exists storables" []
    run con "create table storables (a integer, b integer, c bigint, d char(100))" []
    return ()

  getFilterA (CLT l) hp = getFilterSTMT (hGetLT hp) [convert l]
  getFilterA (CGT g) hp = getFilterSTMT (hGetGT hp) [convert g]
  getFilterA (CLGT l g) hp = getFilterSTMT (hGetLGT hp) [convert l,
                                                         convert g]

getFilterSTMT st vals = do
  execute st vals
  vls <- fetchAllRows st
  return $ map fromSqlVal vls


