{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Cardano.Prelude
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           System.Mem (performMajorGC)

main :: IO ()
main = do
  queue <- atomically $ TBQ.newTBQueue 2048
  let qProc = do
        nlis <- atomically $ do
                      r <- TBQ.flushTBQueue queue
                      when (null r) retry
                      return r
        res <- mapM (const $ pure True) nlis
        when (and res) $ qProc
  dispatcher <- Async.async qProc

  -- The Async.link is necessary
  Async.link dispatcher

  threadDelay 1000
  performMajorGC
  threadDelay 1000
