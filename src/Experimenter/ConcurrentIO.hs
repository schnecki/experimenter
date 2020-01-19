{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Experimenter.ConcurrentIO
    ( doFork
    , collectForkResult
    , mapConurrentIO
    ) where

import           Control.Concurrent      (forkIO, threadDelay, yield)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.DeepSeq
import           Control.Monad           (void)
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Maybe              (fromJust)
import           System.IO

mapConurrentIO :: (NFData b) => Int -> (a -> IO b) -> [a] -> IO [b]
mapConurrentIO maxNr f xs = do
  nr <- newTMVarIO 0
  mapConurrentIO' nr maxNr f xs

mapConurrentIO' :: (NFData b) => TMVar Int -> Int -> (a -> IO b) -> [a] -> IO [b]
mapConurrentIO' _ _ _ [] = return []
mapConurrentIO' tmVar maxNr f (x:xs) = do
  nr <- fmap fromJust $ atomically $ tryReadTMVar tmVar
  -- putStrLn ("Nr: " ++ show nr) >> hFlush stdout
  if nr >= maxNr
    then atomically (readTMVar tmVar) >> mapConurrentIO' tmVar maxNr f (x : xs)
    else do
      increase
      !xThread <- doFork $ f x >>= (\v -> decrease >> return v)
      xs' <- mapConurrentIO' tmVar maxNr f xs
      x' <- collectForkResult xThread
      return (x' : xs')
  where
    increase = modify (+ 1)
    decrease = modify (subtract 1)
    modify g =
      void $
      atomically $ do
        nr <- fromJust <$> tryReadTMVar tmVar
        swapTMVar tmVar (g nr)

doFork :: NFData a => IO a -> IO (IORef (ThreadState a))
doFork f = do
  ref <- newIORef NotReady
  void $ forkIO (f >>= writeIORef ref . Ready . force)
  return ref

collectForkResult :: IORef (ThreadState a) -> IO a
collectForkResult ref = do
  mRes <- readIORef ref
  case mRes of
    NotReady -> yield >> collectForkResult ref
    Ready a  -> return a

data ThreadState a = NotReady | Ready a
