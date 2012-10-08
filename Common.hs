{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Common where

import Data.Int
import Data.List (foldl')
import Data.Text (Text, pack)
import System.Random
import Control.DeepSeq
import Control.Monad.Trans.Error
import Control.Monad.Trans
import Control.Monad.IO.Class
import Data.Time.Clock
import Control.Monad.Trans.Control

data Storable = Storable Int32 Int32 Int64 Text
              deriving (Eq, Show)

instance Random Storable where
  random g = (Storable a b c d, gn)
    where
      (a, g1) = randomR (0, 1000) g
      (b, g2) = random g1
      (c, gn) = random g2
      d = pack $ take 10 $ randomRs ('a', 'z') gn
  randomR _ g = random g        --  FIXME: we just dont need it now

instance NFData Storable where
  rnf (Storable a b c d) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` ()

genStorables :: IO [Storable]
genStorables = do
  g <- newStdGen
  return $ randoms g

data Condition = CLT Int32
               | CGT Int32
               | CLGT Int32 Int32
  
class Storage a where
  saveS :: (MonadIO m, MonadBaseControl IO m, Monad m) => a -> [Storable] -> ErrorT String m ()
  getS :: (Monad m, MonadBaseControl IO m, MonadIO m) => a -> ErrorT String m [Storable]
  getFilterA :: (MonadIO m, MonadBaseControl IO m, Monad m) => Condition -> a -> ErrorT String m [Storable]
  resetS :: (Monad m, MonadBaseControl IO m, MonadIO m) => a -> ErrorT String m ()

measureTime :: (Monad m, MonadBaseControl IO m, MonadIO m, NFData a) => (a -> String) -> ErrorT String m a -> ErrorT String m a
measureTime s m = do
  t1 <- liftIO getCurrentTime
  res <- t1 `deepseq` m
  t2 <- res `deepseq` liftIO getCurrentTime
  liftIO $ putStrLn $ (s res) ++ " took " ++ (show $ diffUTCTime t2 t1) ++ " seconds"
  return res

beforeTest :: (Storage a, Monad m, MonadBaseControl IO m, MonadIO m) => Int -> a -> ErrorT String m ()
beforeTest inscount a = do
  resetS a
  measureTime (\_ -> "Inserting " ++ (show inscount) ++ " elements") $ do
    x <- liftIO genStorables
    saveS a $ take inscount x
  return ()

littleTest :: (Storage a, Monad m, MonadBaseControl IO m, MonadIO m) => Int -> a -> ErrorT String m ()
littleTest inscount a = do
  beforeTest inscount a
  measureTime (\x -> "Fetching " ++ (show inscount) ++ " elemtns") $ getS a
  return ()

copyTest :: (Storage a, Monad m, MonadBaseControl IO m, MonadIO m) => Int -> a -> ErrorT String m ()
copyTest i a = do
  beforeTest i a
  measureTime (\x -> "Copying " ++ (show i) ++ " elements") $ do
    g <- getS a
    saveS a g
  return ()

aggTest :: (Storage a, Monad m, MonadBaseControl IO m, MonadIO m) => Int -> a -> ErrorT String m ()
aggTest i a = do
  beforeTest i a
  measureTime (\_ -> "Aggregating " ++ (show i) ++ " elements") $ do
    g <- getS a
    saveS a [aggMax g]

splitList :: Int -> [a] -> [[a]]
splitList i x = a : (stop b $ splitList i b)
  where
    (a, b) = splitAt i x
    stop [] _ = []
    stop _ x = x

aggMax :: [Storable] -> Storable
aggMax x = foldl' fl (Storable 0 0 0 "") x
  where
    fl (Storable a b c d) (Storable aa bb cc dd) = Storable (max a aa) (max b bb) (max c cc) (max d dd)
  
mapLeftE :: (Monad m) => (a -> b) -> ErrorT a m c -> ErrorT b m c
mapLeftE f m = ErrorT $ runErrorT m >>= return . mapl
    where
      mapl (Left x) = Left $ f x
      mapl (Right x) = Right x

maybeToErrorT :: (Monad m, Error b) => b -> Maybe a -> ErrorT b m a
maybeToErrorT _ (Just x) = return x
maybeToErrorT b Nothing = throwError b