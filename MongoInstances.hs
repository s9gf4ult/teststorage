{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module MongoInstances where

import Database.MongoDB
import Common
import Control.Monad.Trans.Error
import Data.Monoid

toDocument :: Storable -> Document
toDocument (Storable a b c d) = ["a" := Int32 a,
                                 "b" := Int32 b,
                                 "c" := Int64 c,
                                 "d" := String d]

fromDocument :: (Monad m) => Document -> ErrorT String m Storable
fromDocument doc = do
  a <- fifi "can not find field a" findA
  b <- fifi "can not find field b" findB
  c <- fifi "can not find field c" findC
  d <- fifi "can not find field d" findD
  return $ Storable a b c d
  where
    fifi st fndr = maybeToErrorT st $ getFirst $ mconcat $ map (First . fndr) doc
    findA ("a" := Int32 a) = Just a
    findA _ = Nothing
    findB ("b" := Int32 b) = Just b
    findB _ = Nothing
    findC ("c" := Int64 c) = Just c
    findC ("c" := Int32 c) = Just $ fromIntegral c
    findC _ = Nothing
    findD ("d" := String s) = Just s
    findD _ = Nothing
    

combineGetS cond a = do
  docs <- mapLeftE show $ ErrorT $ access a slaveOk "test" $ do
    c <- find $ select cond "storables"
    rest c
  mapM fromDocument docs

instance Storage Pipe where
  saveS a vals = mapLeftE show $ ErrorT $ access a master "test" $
                 mapM_ (insertMany_ "storables") $ splitList 400 $ map toDocument vals
  getS a = combineGetS [] a

  getFilterA (CLT lt) a = combineGetS ["a" := Doc ["$lt" := Int32 lt]] a
  getFilterA (CGT gt) a = combineGetS ["a" := Doc ["$gt" := Int32 gt]] a
  getFilterA (CLGT lt gt) a = combineGetS ["a" := Doc ["$lt" := Int32 lt,
                                                       "$gt" := Int32 gt]] a

  resetS a = mapLeftE show $ ErrorT $ access a master "test" $
             delete $ select [] "storables"