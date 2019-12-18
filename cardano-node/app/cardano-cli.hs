{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Cardano.BM.Backend.Switchboard
import qualified Cardano.BM.Configuration.Model as Config
import           Cardano.BM.Setup
import           Cardano.BM.Data.Trace
import           Cardano.Prelude
import           Control.Concurrent (threadDelay)
import           System.Mem (performMajorGC)

main :: IO ()
main = do
  _ :: (Trace IO Text, Switchboard Text)
    <- flip setupTrace_ "" =<< Config.empty

  threadDelay 1000
  performMajorGC
  threadDelay 1000
  putStrLn ("waiting" :: Text)
  threadDelay 100000000
