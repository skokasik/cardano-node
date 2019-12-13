module Cardano.Config.Defaults
  ( muteTracing
  , traceOptionsDefault
  ) where

import           Cardano.Prelude

import           Cardano.Config.Types

import           Cardano.BM.Data.Tracer (TracingVerbosity (..))


traceOptionsDefault :: TraceOptions
traceOptionsDefault = TraceOptions
  { traceVerbosity = NormalVerbosity
  , traceChainDB = True
  , traceChainSyncClient = True
  , traceChainSyncHeaderServer = True
  , traceChainSyncBlockServer = True
  , traceBlockFetchDecisions = True
  , traceBlockFetchClient = True
  , traceBlockFetchServer = True
  , traceTxInbound = True
  , traceTxOutbound = True
  , traceLocalTxSubmissionServer = True
  , traceMempool = True
  , traceForge = True
  , traceChainSyncProtocol = True
  , traceBlockFetchProtocol = True
  , traceBlockFetchProtocol' = True
  , traceTxSubmissionProtocol = True
  , traceLocalChainSyncProtocol = True
  , traceLocalTxSubmissionProtocol = True
  , traceIpSubscription = True
  , traceDnsSubscription = True
  , traceDnsResolver = True
  , traceErrorPolicy = True
  , traceMux = True
  }

muteTracing :: TraceOptions
muteTracing = TraceOptions
  { traceVerbosity = NormalVerbosity
  , traceChainDB = False
  , traceChainSyncClient = False
  , traceChainSyncHeaderServer = False
  , traceChainSyncBlockServer = False
  , traceBlockFetchDecisions = False
  , traceBlockFetchClient = False
  , traceBlockFetchServer = False
  , traceTxInbound = False
  , traceTxOutbound = False
  , traceLocalTxSubmissionServer = False
  , traceMempool = False
  , traceForge = False
  , traceChainSyncProtocol = False
  , traceBlockFetchProtocol = False
  , traceBlockFetchProtocol' = False
  , traceTxSubmissionProtocol = False
  , traceLocalChainSyncProtocol = False
  , traceLocalTxSubmissionProtocol = False
  , traceIpSubscription = False
  , traceDnsSubscription = False
  , traceDnsResolver = False
  , traceErrorPolicy = False
  , traceMux = False
  }
