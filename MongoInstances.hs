{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

import Database.MongoDB
import Common
import Control.Monad.Trans.Error

toDocument :: Storable -> Document
toDocument = undefined

fromDocument :: Document -> ErrorT String m Storable
fromDocument = undefined

combineGetS cond a = do
  docs <- mapLeftE show $ ErrorT $ access a slaveOk "test" $ do
    c <- find $ select cond "storables"
    rest c
  mapM fromDocument docs

instance Storage Pipe where
  saveS a vals = mapLeftE show $ ErrorT $ access a master "test" $
                 insertMany_ "storables" $ map toDocument vals
  getS a = combineGetS [] a

  getFilterA (CLT lt) a = combineGetS ["a" := Doc ["$lt" := Int32 lt]] a
  getFilterA (CGT gt) a = combineGetS ["a" := Doc ["$gt" := Int32 gt]] a
  getFilterA (CLGT lt gt) a = combineGetS ["a" := Doc ["$lt" := Int32 lt,
                                                       "$gt" := Int32 gt]] a

  resetS a = mapLeftE show $ ErrorT $ access a master "test" $
             delete $ select [] "storables"