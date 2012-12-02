{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
module PostInstance where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Control.Monad.Trans.Error
import Control.DeepSeq
import Control.Exception
import Common
import Control.Monad.IO.Class
import Data.List (splitAt)

instance ToRow Storable where
  toRow (Storable a b c d) = [toField a,
                              toField b,
                              toField c,
                              toField d]

instance FromRow Storable where
  fromRow = do
    a <- field
    b <- field
    c <- field
    d <- field
    return $ Storable a b c d

execLeft :: (Monad m, MonadIO m) => IO a -> ErrorT String m a 
execLeft m = (mapLeftE (show :: SomeException -> String)) $ ErrorT $ liftIO $ try $ (m >>= evaluate)

instance (Monad m, MonadIO m) => Storage Connection (ErrorT String m) where
  saveS c s = do
    execLeft $ mapM_ (executeMany c "insert into storables(a, b, c, d) values (?,?,?,?)") $ splitList 1000 s
    return ()
  getS c = execLeft $ query_ c "select a, b, c, d from storables"
  resetS c = execLeft $ do
    mapM_ (execute_ c) ["drop table if exists storables",
                        "create table storables (a integer, b integer, c bigint, d char(100))",
                        "create index on storables (a)",
                        "create index on storables (b)",
                        "create index on storables (c)",
                        "create index on storables (d)"]

  getFilterA (CLT l) c = execLeft $ query c "select a, b, c, d from storables where a < ?" [l]
  getFilterA (CGT r) c = execLeft $ query c "select a, b, c, d from storables where a > ?" [r]
  getFilterA (CLGT l r) c = execLeft $ query c "select a, b, c, d from storables where a > ? and a < ?" (l, r)
