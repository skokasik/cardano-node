{-# LANGUAGE ScopedTypeVariables #-}

import           Cardano.Prelude hiding (trace)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           System.Mem (performMajorGC)
import           GHC.IO.Exception (BlockedIndefinitelyOnSTM)
import           Control.Exception (Exception)
import           Data.Typeable (cast, typeOf)

import Prelude (error)
import Debug.Trace (trace)

isBlockedIndefinitelyOnSTM :: SomeException -> Bool
isBlockedIndefinitelyOnSTM (SomeException e) =
  isJust (cast e :: Maybe BlockedIndefinitelyOnSTM)

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

  Async.linkOnly (not . isBlockedIndefinitelyOnSTM) dispatcher

  threadDelay 1000
  performMajorGC
  threadDelay 1000
