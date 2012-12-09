module HdbcTest where

import Common
import HdbcInstances
import System.Environment
import Control.Monad.Trans.Error
import Database.HDBC
import Database.HDBC.PostgreSQL

  

makePost = do
  a <- connectPostgreSQL "host=127.0.0.1 user=test password=test dbname=test"
  ins <- prepare a "insert into storables (a, b, c, d) values (?,?,?,?)"
  get <- prepare a "select a, b, c, d from storables"
  getLT <- prepare a "select a, b, c, d from storables where a < ?"
  getGT <- prepare a "select a, b, c, d from storables where a > ?"
  getLGT <- prepare a "select a, b, c, d from storables where a < ? and a > ?"
  return $ HDBCPack {hConn = ConnWrapper a
                    ,hInsert = ins
                    ,hGet = get
                    ,hGetLT = getLT
                    ,hGetGT = getGT
                    ,hGetLGT = getLGT}

main = do
  args <- getArgs
  c <- makePost
  argsExec args c
  return ()