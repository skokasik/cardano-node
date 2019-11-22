{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.Node.Run
  ( runNode
  , ViewMode(..)
  )
where

import           Cardano.Prelude hiding (ByteString, atomically, take, trace)
import           Prelude (error, id, unlines)

import qualified Control.Concurrent.Async as Async
import           Control.Exception (IOException)
import qualified Control.Exception as Exception
import           Control.Tracer
import           Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LB
import           Data.Either (partitionEithers)
import           Data.Functor.Contravariant (contramap)
import qualified Data.List as List
import           Data.Proxy (Proxy (..))
import           Data.Semigroup ((<>))
import           Data.Text (Text, breakOn, pack, take)
import           Network.Socket as Socket
import           Network.HostName (getHostName)
import           System.Directory (canonicalizePath, makeAbsolute)

import           Control.Monad.Class.MonadSTM

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.BackendKind (BackendKind (TraceForwarderBK))
import           Cardano.BM.Data.LogItem (LogObject (..))
import           Cardano.BM.Data.Tracer (ToLogObject (..),
                     TracingVerbosity (..), setHostname)
import           Cardano.Config.Logging (LoggingLayer (..))
import           Cardano.Config.Types (MiscellaneousFilepaths(..),
                                       NodeConfiguration (..), ViewMode (..))

import           Ouroboros.Network.Block
import           Ouroboros.Network.Subscription.Dns

import           Ouroboros.Consensus.Node (NodeKernel (getChainDB),
                     RunNetworkArgs (..),
                     RunNode (nodeNetworkMagic, nodeStartTime))
import qualified Ouroboros.Consensus.Node as Node (run)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import qualified Ouroboros.Consensus.Protocol as Consensus
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.STM (onEachChange)

import qualified Ouroboros.Storage.ChainDB as ChainDB

import           Cardano.Common.LocalSocket
import           Cardano.Config.Protocol (SomeProtocol(..), fromProtocol)
import           Cardano.Config.Topology
import           Cardano.Config.Types (ConfigYamlFilePath(..), DbFile(..), NodeMockCLI(..),
                                       NodeProtocolMode (..), NodeRealCLI(..),
                                       SocketFile(..), TopologyFile(..),
                                       parseNodeConfiguration)
import           Cardano.Tracing.Tracers
#ifdef UNIX
import           Cardano.Node.TUI.LiveView
#endif


-- | Peer identifier used in consensus application
--
data Peer = Peer { localAddr  :: SockAddr
                 , remoteAddr :: SockAddr }
  deriving (Eq, Ord, Show)

instance Condense Peer where
    condense (Peer localA remoteA) = (show localA) ++ (show remoteA)

instance NoUnexpectedThunks Peer where
    showTypeOf _ = "Peer"
    whnfNoUnexpectedThunks _ctxt _act = return NoUnexpectedThunks


runNode
  :: LoggingLayer
  -> NodeProtocolMode
  -> IO ()
runNode loggingLayer npm = do
    hn <- hostname
    let !trace = setHostname hn $
                 llAppendName loggingLayer "node" (llBasicTrace loggingLayer)
    let tracer = contramap pack $ toLogObject trace

    (mscFp', configFp', traceOpts') <-
        case npm of
          MockProtocolMode (NodeMockCLI mMscFp _ configYaml mTraceOpts) ->
            pure (mMscFp, configYaml, mTraceOpts)
          RealProtocolMode (NodeRealCLI rMscFp _ configYaml rTraceOpts) ->
            pure (rMscFp, configYaml, rTraceOpts)

    nc <- parseNodeConfiguration $ unConfigPath configFp'
    --TODO: Two separate yaml formats? Real vs Mock? Need to change ConfigYaml newtype
    traceWith tracer $ "tracing verbosity = " ++
                         case traceVerbosity traceOpts' of
                             NormalVerbosity -> "normal"
                             MinimalVerbosity -> "minimal"
                             MaximalVerbosity -> "maximal"
    SomeProtocol p  <- fromProtocol
                         (ncGenesisHash nc)
                         (ncNodeId nc)
                         (ncNumCoreNodes nc)
                         (genesisFile mscFp')
                         (ncReqNetworkMagic nc)
                         (ncPbftSignatureThresh nc)
                         (delegCertFile mscFp')
                         (signKeyFile mscFp')
                         (ncUpdate nc)
                         (ncProtocol nc)

    let tracers     = mkTracers traceOpts' trace

    case ncViewMode nc of
      SimpleView -> handleSimpleNode p trace tracers npm
      LiveView   -> do
#ifdef UNIX
        let c = llConfiguration loggingLayer
        -- We run 'handleSimpleNode' as usual and run TUI thread as well.
        -- turn off logging to the console, only forward it through a pipe to a central logging process
        CM.setDefaultBackends c [TraceForwarderBK, UserDefinedBK "LiveViewBackend"]
        -- User will see a terminal graphics and will be able to interact with it.
        nodeThread <- Async.async $ handleSimpleNode p trace tracers npm

        be :: LiveViewBackend Text <- realize c
        let lvbe = MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be }
        llAddBackend loggingLayer lvbe (UserDefinedBK "LiveViewBackend")
        let nId = fromMaybe (panic "LiveView not possible for real protocols as yet") (ncNodeId nc)
        setTopology be nId
        setNodeThread be nodeThread
        captureCounters be trace

        void $ Async.waitAny [nodeThread]
#else
        handleSimpleNode p trace tracers npm
#endif
  where
    hostname = do
      hn0 <- pack <$> getHostName
      return $ take 8 $ fst $ breakOn "." hn0

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.
handleSimpleNode :: forall blk. RunNode blk
                 => Consensus.Protocol blk
                 -> Tracer IO (LogObject Text)
                 -> Tracers Peer blk
                 -> NodeProtocolMode
                 -> IO ()
handleSimpleNode p trace nodeTracers npm = do
  case npm of
    -- Run a node using a real protocol
    RealProtocolMode (NodeRealCLI rMscFp rNodeAddr _ _) -> do
      let pInfo@ProtocolInfo{ pInfoConfig = cfg } = protocolInfo p

      -- Topology
      eitherTopology <- readRealNodeTopology . unTopology $ topFile rMscFp
      topology <- case eitherTopology of
                    --TODO: Convert handleSimpleNode to return `ExceptT`
                    Left err -> panic $ "Cardano.Node.Run.readRealNodeTopology: "
                                      <> err
                    Right top -> pure top

      -- Tracing
      let tracer = contramap pack $ toLogObject trace
      traceWith tracer $ unlines
        [ ""
        , "**************************************"
        , "Host node address: " <> (show $ rNodeAddress topology)
        , "My producers are " <> (show $ rProducers topology)
        , "**************************************"
        ]


      -- Socket directory
      myLocalAddr <- localSocketAddrInfo
                       Nothing
                       (unSocket $ socketFile rMscFp)
                       MkdirIfMissing

      addrs <- nodeAddressInfo rNodeAddr

      let ipProducerAddrs  :: [NodeAddress]
          --TODO:I believe I need to get producers via jq conversion of topology file
          dnsProducerAddrs :: [RemoteAddress]
          (ipProducerAddrs, dnsProducerAddrs) = partitionEithers
            [ maybe (Right ra) Left $ remoteAddressToNodeAddress ra
            | ra <- rProducers topology ]
          ipProducers :: [SockAddr]
          ipProducers = nodeAddressToSockAddr <$> ipProducerAddrs


          dnsProducers :: [DnsSubscriptionTarget]
          dnsProducers = producerSubscription <$> dnsProducerAddrs


          runNetworkArgs :: RunNetworkArgs Peer blk
          runNetworkArgs = RunNetworkArgs
            { rnaIpSubscriptionTracer  = ipSubscriptionTracer  nodeTracers
            , rnaDnsSubscriptionTracer = dnsSubscriptionTracer nodeTracers
            , rnaDnsResolverTracer     = dnsResolverTracer     nodeTracers
            , rnaErrorPolicyTracer     = errorPolicyTracer     nodeTracers
            , rnaMuxTracer             = muxTracer             nodeTracers
            , rnaMuxLocalTracer        = nullTracer
            , rnaMkPeer                = Peer
            , rnaMyAddrs               = addrs
            , rnaMyLocalAddr           = myLocalAddr
            , rnaIpProducers           = ipProducers
            , rnaDnsProducers          = dnsProducers
            , rnaHandshakeTracer       = nullTracer
            , rnaHandshakeLocalTracer  = nullTracer
            , rnaNetworkMagic          = nodeNetworkMagic (Proxy @blk) cfg
            }

          producerSubscription :: RemoteAddress -> DnsSubscriptionTarget
          producerSubscription ra =
            DnsSubscriptionTarget
            { dstDomain  = BSC.pack (raAddress ra)
            , dstPort    = raPort ra
            , dstValency = raValency ra
            }


      removeStaleLocalSocket Nothing (unSocket $ socketFile rMscFp )
      dbPath <- canonicalizePath =<< makeAbsolute (unDB $ dBFile rMscFp)
      varTip <- atomically $ newTVar GenesisPoint


      Node.run
        (consensusTracers nodeTracers)
        (withTip varTip $ chainDBTracer nodeTracers)
        runNetworkArgs
        dbPath
        pInfo
        id -- No ChainDbArgs customisation
        id -- No NodeParams customisation
        $ \registry nodeKernel -> do
          -- Watch the tip of the chain and store it in @varTip@ so we can include
          -- it in trace messages.
          let chainDB = getChainDB nodeKernel
          onEachChange registry id Nothing (ChainDB.getTipPoint chainDB) $ \tip ->
            atomically $ writeTVar varTip tip


    MockProtocolMode (NodeMockCLI mMscFp mockNodeAddr cfgYaml _) -> do
                    nc <- parseNodeConfiguration $ unConfigPath cfgYaml
                    NetworkTopology nodeSetups <- either error id <$> readTopologyFile (unTopology $ topFile mMscFp)

                    let pInfo@ProtocolInfo{ pInfoConfig = cfg } = protocolInfo p

                    -- Tracing
                    let tracer = contramap pack $ toLogObject trace
                    traceWith tracer $ "System started at " <> show (nodeStartTime (Proxy @blk) cfg)

                    let producersList = map (\ns -> (nodeId ns, producers ns)) nodeSetups

                    let producers' = case (List.lookup (nid nc) producersList) of
                                       Just ps ->  ps
                                       Nothing -> error $ "handleSimpleNode: own address "
                                                     <> show mockNodeAddr
                                                     <> ", Node Id "
                                                     <> show (nid nc)
                                                     <> " not found in topology"

                    ----------------------------------------------

                    traceWith tracer $ unlines
                      [ "**************************************"
                      , "I am Node "        <> show mockNodeAddr <> " Id: " <> show (nid nc)
                      , "My producers are " <> show producers'
                      , "**************************************"
                      ]

                    -- Socket directory
                    myLocalAddr <- localSocketAddrInfo
                                      (ncNodeId nc)
                                      (unSocket $ socketFile mMscFp)
                                      MkdirIfMissing

                    addrs <- nodeAddressInfo mockNodeAddr
                    let ipProducerAddrs  :: [NodeAddress]
                        --TODO: Need to figure out how to get producers based on real vs mock
                        dnsProducerAddrs :: [RemoteAddress]
                        (ipProducerAddrs, dnsProducerAddrs) = partitionEithers
                          [ maybe (Right ra) Left $ remoteAddressToNodeAddress ra
                          | ra <- producers' ]
                        ipProducers :: [SockAddr]
                        ipProducers = nodeAddressToSockAddr <$> ipProducerAddrs
                        dnsProducers :: [DnsSubscriptionTarget]
                        dnsProducers = producerSubscription <$> dnsProducerAddrs
                        runNetworkArgs :: RunNetworkArgs Peer blk
                        runNetworkArgs = RunNetworkArgs
                          { rnaIpSubscriptionTracer  = ipSubscriptionTracer  nodeTracers
                          , rnaDnsSubscriptionTracer = dnsSubscriptionTracer nodeTracers
                          , rnaDnsResolverTracer     = dnsResolverTracer     nodeTracers
                          , rnaErrorPolicyTracer     = errorPolicyTracer     nodeTracers
                          , rnaMuxTracer             = muxTracer             nodeTracers
                          , rnaMuxLocalTracer        = nullTracer
                          , rnaMkPeer                = Peer
                          , rnaMyAddrs               = addrs
                          , rnaMyLocalAddr           = myLocalAddr
                          , rnaIpProducers           = ipProducers
                          , rnaDnsProducers          = dnsProducers
                          , rnaHandshakeTracer       = nullTracer
                          , rnaHandshakeLocalTracer  = nullTracer
                          , rnaNetworkMagic          = nodeNetworkMagic (Proxy @blk) cfg
                          }
                        producerSubscription :: RemoteAddress -> DnsSubscriptionTarget
                        producerSubscription ra =
                          DnsSubscriptionTarget
                          { dstDomain  = BSC.pack (raAddress ra)
                          , dstPort    = raPort ra
                          , dstValency = raValency ra
                          }

                    removeStaleLocalSocket (ncNodeId nc) (unSocket $ socketFile mMscFp)
                    dbPath <- canonicalizePath =<< makeAbsolute (unDB $ dBFile mMscFp)

                    varTip <- atomically $ newTVar GenesisPoint
                    Node.run
                      (consensusTracers nodeTracers)
                      (withTip varTip $ chainDBTracer nodeTracers)
                      runNetworkArgs
                      (dbPath <> "-" <> show (nid nc))
                      pInfo
                      id -- No ChainDbArgs customisation
                      id -- No NodeParams customisation
                      $ \registry nodeKernel -> do
                        -- Watch the tip of the chain and store it in @varTip@ so we can include
                        -- it in trace messages.
                        let chainDB = getChainDB nodeKernel
                        onEachChange registry id Nothing (ChainDB.getTipPoint chainDB) $ \tip ->
                          atomically $ writeTVar varTip tip
  where
      --TODO: Make it pattern match on CoreId/RelayId, remove the Just.
      -- You can only do this once you have two separate config yaml files for real vs mock
      nid :: NodeConfiguration -> Int
      nid nc = case ncNodeId nc of
              Just (CoreId  n) -> n
              Just (RelayId _) -> error "Non-core nodes currently not supported"
              Nothing -> 999

-- | Read the `RealNodeTopology` configuration from the specified file.
-- While running a real protocol, this gives your node its own address and
-- other remote peers it will attempt to connect to.
readRealNodeTopology :: FilePath -> IO (Either Text RealNodeTopology)
readRealNodeTopology fp = do
  ebs <- Exception.try $ BSC.readFile fp :: IO (Either IOException BSC.ByteString)
  case ebs of
    Left e -> pure $ handler e
    Right bs -> pure . first toS . eitherDecode $ LB.fromStrict bs
 where
   handler :: IOException -> Either Text RealNodeTopology
   handler e =  Left . pack $ show e
