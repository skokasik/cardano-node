{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Run (
      runNode
    ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.Serialise as Serialise (decode, encode)
import           Codec.SerialiseTerm
import qualified Control.Concurrent.Async as Async
import           Control.Exception
import           Control.Monad
import           Control.Tracer
import           Crypto.Random
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor.Contravariant (contramap)
import qualified Data.List as List
import           Data.Proxy (Proxy (..))
import           Data.Semigroup ((<>))
import           Data.Text (Text, pack)
import           Data.Time.Clock (DiffTime, secondsToDiffTime)
import           Network.Socket as Socket
import           System.Directory (removeFile)
import           System.IO.Error (isDoesNotExistError)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM

import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (Trace, appendName)

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode as NodeToNode
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Subscription.Common

import           Ouroboros.Network.Protocol.BlockFetch.Codec
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec

import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.NodeNetwork
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.STM
import           Ouroboros.Consensus.Util.ThreadRegistry

import           Ouroboros.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.Common
import           Ouroboros.Storage.ImmutableDB (ValidationPolicy (..))
import           Ouroboros.Storage.LedgerDB.DiskPolicy (defaultDiskPolicy)
import           Ouroboros.Storage.LedgerDB.MemPolicy (defaultMemPolicy)
import qualified Ouroboros.Storage.LedgerDB.OnDisk as LedgerDB

import           Cardano.Node.CLI
import           CLI
import           LiveView
import           Topology
import           TraceAcceptor
import           TxSubmission


-- | Peer identifier used in consensus application
--
data Peer = Peer { localAddr  :: SockAddr
                 , remoteAddr :: SockAddr }
  deriving (Eq, Ord, Show)

instance Condense Peer where
    condense (Peer localAddr remoteAddr) = (show localAddr) ++ (show remoteAddr)


runNode :: NodeCLIArguments -> Trace IO Text -> IO ()
runNode nodeCli@NodeCLIArguments{..} trace = do
    -- If the user asked to submit a transaction, we don't have to spin up a
    -- full node, we simply transmit it and exit.
    case command of

      TxSubmitter topology tx protocol -> do
        let trace'      = appendName (pack (show (node topology))) trace
        let tracer      = contramap pack $ toLogObject trace'
        SomeProtocol p  <- fromProtocol protocol
        handleTxSubmission p topology tx tracer

      TraceAcceptor -> do
        let trace'      = appendName "acceptor" trace
        let tracer      = contramap pack $ toLogObject trace'
        handleTraceAcceptor tracer

      SimpleNode topology myNodeAddress protocol viewMode -> do
        let trace'      = appendName (pack $ show $ node topology) trace
        let tracer      = contramap pack $ toLogObject trace'
        SomeProtocol p  <- fromProtocol protocol
        case viewMode of
          SimpleView -> handleSimpleNode p nodeCli myNodeAddress topology tracer
          LiveView   -> do
            -- We run 'handleSimpleNode' as usual and run TUI thread as well.
            -- User will see a terminal graphics and will be able to interact with it.
            nodeThread <- Async.async $ handleSimpleNode p nodeCli myNodeAddress topology tracer
            tuiThread  <- Async.async $ runNodeLiveView topology
            _ <- Async.waitAny [nodeThread, tuiThread]
            return ()

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.
handleSimpleNode :: forall blk. RunDemo blk
                 => DemoProtocol blk
                 -> NodeCLIArguments
                 -> NodeAddress
                 -> TopologyInfo
                 -> Tracer IO String
                 -> IO ()
handleSimpleNode p NodeCLIArguments{..} myNodeAddress (TopologyInfo myNodeId topologyFile) tracer = do
    traceWith tracer $ "System started at " <> show systemStart
    NetworkTopology nodeSetups <-
      either error id <$> readTopologyFile topologyFile

    let producers' = case List.lookup myNodeAddress $ map (\ns -> (nodeAddress ns, producers ns)) nodeSetups of
          Just ps -> ps
          Nothing -> error "handleSimpleNode: own address not found in topology"

    traceWith tracer $ "**************************************"
    traceWith tracer $ "I am Node = " <> show myNodeAddress
    traceWith tracer $ "My producers are " <> show producers'
    traceWith tracer $ "**************************************"

    let ProtocolInfo{pInfoConfig, pInfoInitLedger, pInfoInitState} =
          protocolInfo (NumCoreNodes (length nodeSetups)) (CoreNodeId nid) p

    withThreadRegistry $ \registry -> do

      let callbacks :: NodeCallbacks IO blk
          callbacks = NodeCallbacks {
              produceDRG   = drgNew
            , produceBlock = \proof _l slot prevPoint prevBlockNo txs -> do
                let curNo :: BlockNo
                    curNo = succ prevBlockNo

                    prevHash :: ChainHash blk
                    prevHash = castHash (Block.pointHash prevPoint)

                 -- The transactions we get are consistent; the only reason not
                 -- to include all of them would be maximum block size, which
                 -- we ignore for now.
                demoForgeBlock pInfoConfig
                               slot
                               curNo
                               prevHash
                               txs
                               proof
          }

      varTip <- atomically $ newTVar GenesisPoint
      let chainDbArgs = mkChainDbArgs pInfoConfig pInfoInitLedger registry varTip
      chainDB :: ChainDB IO blk <- ChainDB.openDB chainDbArgs

      -- Watch the tip of the Chain and store it in varTip
      onEachChange registry id GenesisPoint (ChainDB.getTipPoint chainDB)
        (atomically . writeTVar varTip)

      btime  <- realBlockchainTime registry slotDuration systemStart
      let nodeParams :: NodeParams IO Peer blk
          nodeParams = NodeParams
            { tracer             = prefixTip varTip tracer
            , mempoolTracer      = contramap show tracer
            , decisionTracer     = nullTracer
            , fetchClientTracer  = nullTracer
            , threadRegistry     = registry
            , maxClockSkew       = ClockSkew 1
            , cfg                = pInfoConfig
            , initState          = pInfoInitState
            , btime
            , chainDB
            , callbacks
            , blockFetchSize     = demoBlockFetchSize
            , blockMatchesHeader = demoBlockMatchesHeader
            }

      kernel <- nodeKernel nodeParams

      let networkApps :: NetworkApplication
                           IO Peer
                           ByteString ByteString
                           ByteString ByteString ()
          networkApps =
            consensusNetworkApps
              nullTracer
              nullTracer
              kernel
              ProtocolCodecs
                { pcChainSyncCodec =
                    codecChainSync
                      (demoEncodeHeader pInfoConfig)
                      (demoDecodeHeader pInfoConfig)
                       encodePoint'
                       decodePoint'

                , pcBlockFetchCodec =
                    codecBlockFetch
                      (demoEncodeBlock pInfoConfig)
                      demoEncodeHeaderHash
                      (demoDecodeBlock pInfoConfig)
                      demoDecodeHeaderHash

                , pcLocalChainSyncCodec =
                    codecChainSync
                      (demoEncodeBlock pInfoConfig)
                      (demoDecodeBlock pInfoConfig)
                       encodePoint'
                       decodePoint'

                , pcLocalTxSubmissionCodec =
                    codecLocalTxSubmission
                      demoEncodeGenTx
                      demoDecodeGenTx
                      Serialise.encode
                      Serialise.decode

                }
              (protocolHandlers nodeParams kernel)

      let networkAppNodeToNode :: Versions
                                    NodeToNodeVersion
                                    DictVersion
                                    (NetworkApplication
                                       IO Peer
                                       ByteString ByteString
                                       ByteString ByteString ())
          networkAppNodeToNode =
            simpleSingletonVersions
              NodeToNodeV_1
              (NodeToNodeVersionData { networkMagic = 0 })
              (DictVersion nodeToNodeCodecCBORTerm)
              networkApps

      let networkAppNodeToClient :: Versions
                                      NodeToClientVersion
                                      DictVersion
                                      (NetworkApplication
                                         IO Peer
                                         ByteString ByteString
                                         ByteString ByteString ())
          networkAppNodeToClient =
            simpleSingletonVersions
              NodeToClientV_1
              (NodeToClientVersionData { networkMagic = 0 })
              (DictVersion nodeToClientCodecCBORTerm)
              networkApps

      myAddr:_ <- case myNodeAddress of
        NodeAddress host port -> getAddrInfo Nothing (Just host) (Just port)

      let myLocalSockPath = localSocketFilePath myNodeId
          myLocalAddr     = localSocketAddrInfo myLocalSockPath
      removeStaleLocalSocket myLocalSockPath

      -- serve local clients (including tx submission)
      localServer <-
        forkLinked registry $ do
          connTable <- newConnectionTable
          NodeToClient.withServer
            connTable
            myLocalAddr
            (\(DictVersion _) -> acceptEq)
            (muxLocalResponderNetworkApplication <$> networkAppNodeToClient)
            wait

      -- serve downstream nodes
      connTable <- newConnectionTable
      peerServer <-
        forkLinked registry $ do
          NodeToNode.withServer
            connTable
            myAddr (\(DictVersion _) -> acceptEq)
            (muxResponderNetworkApplication <$> networkAppNodeToNode)
            wait

      -- ip subscription manager
      subManager <- forkLinked registry $
        ipSubscriptionWorker
          connTable
          (contramap show tracer)
          -- IPv4 address
          --
          -- We can't share portnumber with our server since we run separate
          -- 'MuxInitiatorApplication' and 'MuxResponderApplication'
          -- applications instead of a 'MuxInitiatorAndResponderApplication'.
          -- This means we don't utilise full duplex connection.
          (Just $ Socket.SockAddrInet 0 0)
          -- no IPv6 address
          Nothing
          (const Nothing)
          (IPSubscriptionTarget {
              ispIps     = map nodeAddressToSockAddr producers',
              ispValency = length producers'
            })
          (\sock -> do
              remoteAddr <- getPeerName sock
              localAddr  <- getSocketName sock
              connectToNode'
                      (\(DictVersion codec) -> encodeTerm codec)
                      (\(DictVersion codec) -> decodeTerm codec)
                      (muxInitiatorNetworkApplication
                          (Peer localAddr remoteAddr) <$> networkAppNodeToNode) sock)
          wait

      void $ Async.waitAny [localServer, peerServer, subManager]

  where

      nid :: Int
      nid = case myNodeId of
              CoreId  n -> n
              RelayId _ -> error "Non-core nodes currently not supported"

      encodePoint' ::  Point blk -> Encoding
      encodePoint' =
          Block.encodePoint demoEncodeHeaderHash

      decodePoint' :: forall s. Decoder s (Point blk)
      decodePoint' =
          Block.decodePoint demoDecodeHeaderHash

      prefixTip :: TVar IO (Point blk) -> Tracer IO String -> Tracer IO String
      prefixTip varTip tr = Tracer $ \msg -> do
          tip <- atomically $ readTVar varTip
          let hash = case pointHash tip of
                GenesisHash -> "genesis"
                BlockHash h -> take 4 (condense h)
          traceWith tr ("[" <> hash <> "] " <> msg)

      mkChainDbArgs :: NodeConfig (BlockProtocol blk)
                    -> ExtLedgerState blk
                    -> ThreadRegistry IO
                    -> TVar IO (Point blk)
                    -> ChainDB.ChainDbArgs IO blk
      mkChainDbArgs cfg initLedger registry varTip = (ChainDB.defaultArgs dbPath)
          { ChainDB.cdbBlocksPerFile    = 10
          , ChainDB.cdbDecodeBlock      = demoDecodeBlock       cfg
          , ChainDB.cdbDecodeChainState = demoDecodeChainState  (Proxy @blk)
          , ChainDB.cdbDecodeHash       = demoDecodeHeaderHash
          , ChainDB.cdbDecodeLedger     = demoDecodeLedgerState cfg
          , ChainDB.cdbEncodeBlock      = demoEncodeBlock       cfg
          , ChainDB.cdbEncodeChainState = demoEncodeChainState  (Proxy @blk)
          , ChainDB.cdbEncodeHash       = demoEncodeHeaderHash
          , ChainDB.cdbEncodeLedger     = demoEncodeLedgerState cfg
          , ChainDB.cdbEpochSize        = demoEpochSize         (Proxy @blk)
          , ChainDB.cdbGenesis          = return initLedger
          , ChainDB.cdbDiskPolicy       = defaultDiskPolicy secParam slotDiffTime
          , ChainDB.cdbIsEBB            = \blk -> if demoIsEBB blk
                                                  then Just (blockHash blk)
                                                  else Nothing
          , ChainDB.cdbMemPolicy        = defaultMemPolicy secParam
          , ChainDB.cdbNodeConfig       = cfg
          , ChainDB.cdbThreadRegistry   = registry
          , ChainDB.cdbTracer           = readableChainDBTracer (prefixTip varTip tracer)
          , ChainDB.cdbValidation       = ValidateMostRecentEpoch
          , ChainDB.cdbGcDelay          = secondsToDiffTime 10
          }
        where
          dbPath = "db-" <> show nid

          secParam = protocolSecurityParam cfg

          -- TODO cleaner way with subsecond precision
          slotDiffTime :: DiffTime
          slotDiffTime = secondsToDiffTime
            (slotLengthToMillisec slotDuration `div` 1000)


removeStaleLocalSocket :: FilePath -> IO ()
removeStaleLocalSocket socketPath =
    removeFile socketPath
      `catch` \e ->
        if isDoesNotExistError e
          then return ()
          else throwIO e

-- Converts the trace events from the ChainDB that we're interested in into
-- human-readable trace messages.
readableChainDBTracer
    :: forall m blk.
       ( Monad m
       , Condense (HeaderHash blk)
       , ProtocolLedgerView blk
       )
    => Tracer m String -> Tracer m (ChainDB.TraceEvent blk)
readableChainDBTracer tracer = Tracer $ \case
    ChainDB.TraceAddBlockEvent ev -> case ev of
      ChainDB.StoreButDontChange   pt -> tr $
        "Ignoring block: " <> condense pt
      ChainDB.TryAddToCurrentChain pt -> tr $
        "Block fits onto the current chain: " <> condense pt
      ChainDB.TrySwitchToAFork pt _   -> tr $
        "Block fits onto some fork: " <> condense pt
      ChainDB.SwitchedToChain _ c     -> tr $
        "Chain changed, new tip: " <> condense (AF.headPoint c)
      ChainDB.AddBlockValidation ev' -> case ev' of
        ChainDB.InvalidBlock err pt -> tr $
          "Invalid block " <> condense pt <> ": " <> show err
        _ -> ignore
      _  -> ignore
    ChainDB.TraceLedgerEvent ev -> case ev of
      ChainDB.InitLog ev' -> traceInitLog ev'
      ChainDB.TookSnapshot snap pt -> tr $
        "Took ledger snapshot " <> show snap <> " at " <> condense pt
      ChainDB.DeletedSnapshot snap -> tr $
        "Deleted old snapshot " <> show snap
    ChainDB.TraceCopyToImmDBEvent ev -> case ev of
      ChainDB.CopiedBlockToImmDB pt -> tr $
        "Copied block " <> condense pt <> " to the ImmutableDB"
      _ -> ignore
    ChainDB.TraceGCEvent ev -> case ev of
      ChainDB.PerformedGC slot       -> tr $
        "Performed a garbage collection for " <> condense slot
      _ -> ignore
    ChainDB.TraceOpenEvent ev -> case ev of
      ChainDB.OpenedDB immTip tip -> tr $
        "Opened with immutable tip at " <> condense immTip <>
        " and tip " <> condense tip
      _ -> ignore
    _ -> ignore
  where
    tr s = traceWith tracer ("ChainDB | " <> s)

    ignore :: m ()
    ignore = return ()

    traceInitLog = \case
      LedgerDB.InitFromGenesis -> tr "Initialised the ledger from genesis"
      LedgerDB.InitFromSnapshot snap tip -> tr $
        "Initialised the ledger from snapshot " <> show snap <> " at " <>
        condense (tipToPoint tip)
      LedgerDB.InitFailure snap _failure initLog -> do
          tr $ "Snapshot " <> show snap <> " invalid"
          traceInitLog initLog
