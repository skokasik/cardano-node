{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Ops
  ( deserialiseDelegateKey
  , getGenesisHash
  , getLocalTip
  , readGenesis
  , serialiseDelegationCert
  , serialiseDelegateKey
  , serialiseGenesis
  , serialisePoorKey
  , serialiseSigningKey
  , withRealPBFT
  , CliError(..)
  , RealPBFTError(..)
  , TxGenError(..)
  ) where

import           Prelude (show, unlines)
import           Cardano.Prelude hiding (catch, option, show)
import           Control.Monad.Trans.Except.Extra (firstExceptT, left)
import           Test.Cardano.Prelude (canonicalEncodePretty)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Formatting as F
import qualified Text.JSON.Canonical as CanonicalJSON

import           Cardano.Binary (DecoderError)
import           Cardano.BM.Data.Tracer (TracingVerbosity (..))
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Crypto (RequiresNetworkMagic, SigningKey (..))
import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Signing as Crypto
import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (nullTracer, stdoutTracer, traceWith)
import           Network.Mux (MuxError)
import           Network.TypedProtocol.Driver (runPeer)
import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Ledger.Byron (ByronBlock, GenTx)
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr)
import           Ouroboros.Consensus.NodeNetwork (ProtocolCodecs(..), protocolCodecs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                   (NodeToClientVersion, mostRecentNetworkProtocolVersion)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo(..), protocolInfo)
import           Ouroboros.Consensus.Node.Run
                   (RunNode(..))
import qualified Ouroboros.Consensus.Protocol as Consensus
import           Ouroboros.Consensus.Util.Condense (Condense)
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import           Ouroboros.Network.Block
                   (HeaderHash, Serialised, Tip)
import           Ouroboros.Network.Codec (Codec)
import           Ouroboros.Network.Mux
                   (AppType(InitiatorApp), OuroborosApplication(..))
import           Ouroboros.Network.NodeToClient
                 (NetworkConnectTracers(..), NodeToClientProtocols(..), NodeToClientVersionData(..)
                 , NodeToClientVersion(NodeToClientV_1), connectTo, localTxSubmissionClientNull
                 , nodeToClientCodecCBORTerm)
import           Ouroboros.Network.Protocol.ChainSync.Client
                   (ChainSyncClient(..), ClientStIdle(..), ClientStNext(..)
                   , chainSyncClientPeer, recvMsgRollForward)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Protocol.Handshake.Version
                   (DictVersion(..), Versions, simpleSingletonVersions)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type
                   (LocalTxSubmission)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client
                   (localTxSubmissionClientPeer)

import           Cardano.Common.LocalSocket
import           Cardano.Config.Protocol
                   (Protocol(..), ProtocolInstantiationError
                   , SomeProtocol(..), fromProtocol)
import           Cardano.Config.Types
import qualified Cardano.CLI.Legacy.Byron as Legacy
import           Cardano.Tracing.ToObjectOrphans (showTip)



deserialiseDelegateKey :: Protocol -> FilePath -> LB.ByteString -> Either CliError SigningKey
deserialiseDelegateKey ByronLegacy fp delSkey =
  case deserialiseFromBytes Legacy.decodeLegacyDelegateKey delSkey of
    Left deSerFail -> Left $ SigningKeyDeserialisationFailed fp deSerFail
    Right (_, Legacy.LegacyDelegateKey sKey ) -> pure sKey
deserialiseDelegateKey RealPBFT fp delSkey =
  case deserialiseFromBytes Crypto.fromCBORXPrv delSkey of
    Left deSerFail -> Left $ SigningKeyDeserialisationFailed fp deSerFail
    Right (_, sKey) -> Right $ SigningKey sKey
deserialiseDelegateKey ptcl _ _ = Left $ ProtocolNotSupported ptcl

getGenesisHash :: GenesisFile -> ExceptT CliError IO Text
getGenesisHash genFile = do
  (_, Genesis.GenesisHash gHash) <- readGenesis genFile
  return $ F.sformat Crypto.hashHexF gHash

-- | Read genesis from a file.
readGenesis :: GenesisFile -> ExceptT CliError IO (Genesis.GenesisData, Genesis.GenesisHash)
readGenesis (GenesisFile fp) = firstExceptT (GenesisReadError fp) $ Genesis.readGenesisData fp

serialiseDelegationCert :: CanonicalJSON.ToJSON Identity a => Protocol -> a -> Either CliError LB.ByteString
serialiseDelegationCert ByronLegacy dlgCert = pure $ canonicalEncodePretty dlgCert
serialiseDelegationCert RealPBFT dlgCert = pure $ canonicalEncodePretty dlgCert
serialiseDelegationCert ptcl _ = Left $ ProtocolNotSupported ptcl

serialiseDelegateKey :: Protocol -> SigningKey -> Either CliError LB.ByteString
serialiseDelegateKey ByronLegacy sk = pure
                                    . toLazyByteString
                                    . Legacy.encodeLegacyDelegateKey
                                    $ Legacy.LegacyDelegateKey sk
serialiseDelegateKey RealPBFT sk = serialiseSigningKey RealPBFT sk
serialiseDelegateKey ptcl _ = Left $ ProtocolNotSupported ptcl

serialiseGenesis ::  Protocol -> Genesis.GenesisData -> Either CliError LB.ByteString
serialiseGenesis ByronLegacy gData = pure $ canonicalEncodePretty gData
serialiseGenesis RealPBFT gData = pure $ canonicalEncodePretty gData
serialiseGenesis ptcl _ = Left $ ProtocolNotSupported ptcl

serialisePoorKey :: Protocol -> Genesis.PoorSecret -> Either CliError LB.ByteString
serialisePoorKey ByronLegacy ps = serialiseSigningKey ByronLegacy $ Genesis.poorSecretToKey ps
serialisePoorKey RealPBFT ps = serialiseSigningKey RealPBFT $ Genesis.poorSecretToKey ps
serialisePoorKey ptcl _ = Left $ ProtocolNotSupported ptcl

serialiseSigningKey :: Protocol -> SigningKey -> Either CliError LB.ByteString
serialiseSigningKey ByronLegacy (SigningKey k) = pure . toLazyByteString $ Crypto.toCBORXPrv k
serialiseSigningKey RealPBFT (SigningKey k) = pure . toLazyByteString $ Crypto.toCBORXPrv k
serialiseSigningKey ptcl _ = Left $ ProtocolNotSupported ptcl

-- | Exception type for all errors thrown by the CLI.
--   Well, almost all, since we don't rethrow the errors from readFile & such.
data CliError
  = CertificateValidationErrors !FilePath ![Text]
  | DelegationError !Genesis.GenesisDelegationError
  | DlgCertificateDeserialisationFailed !FilePath !Text
  | GenerateTxsError !RealPBFTError
  | GenesisGenerationError !Genesis.GenesisDataGenerationError
  | GenesisReadError !FilePath !Genesis.GenesisDataError
  | GenesisSpecError !Text
  | IssueUtxoError !RealPBFTError
  | NodeSubmitTxError !RealPBFTError
  | NotEnoughTxInputs
  | NotEnoughTxOutputs
  | NoGenesisDelegationForKey !Text
  | OutputMustNotAlreadyExist !FilePath
  | ProtocolError ProtocolInstantiationError
  | ProtocolNotSupported !Protocol
  | ProtocolParametersParseFailed !FilePath !Text
  | ReadSigningKeyFailure !FilePath !Text
  | ReadVerificationKeyFailure !FilePath !Text
  | TxDeserialisationFailed !FilePath !DecoderError
  -- TODO:  sadly, VerificationKeyParseError isn't exported from Cardano.Crypto.Signing/*
  | SigningKeyDeserialisationFailed !FilePath !DeserialiseFailure
  | SpendGenesisUTxOError !RealPBFTError
  | VerificationKeyDeserialisationFailed !FilePath !Text


instance Show CliError where
  show (OutputMustNotAlreadyExist fp)
    = "Output file/directory must not already exist: " <> fp
  show NotEnoughTxInputs
    = "Transactions must have at least one input."
  show NotEnoughTxOutputs
    = "Transactions must have at least one output."
  show (ProtocolNotSupported proto)
    = "Unsupported protocol "<> show proto
  show (CertificateValidationErrors fp errs)
    = unlines $
      "Errors while validating certificate '" <> fp <> "':":
      (("  " <>) . T.unpack <$> errs)
  show (ProtocolError err)
    = "Protocol Instantiation Error " <> show err
  show (ProtocolParametersParseFailed fp err)
    = "Protocol parameters file '" <> fp <> "' read failure: "<> T.unpack err
  show (GenesisReadError fp err)
    = "Genesis file '" <> fp <> "' read failure: "<> show err
  show (SigningKeyDeserialisationFailed fp err)
    = "Signing key '" <> fp <> "' read failure: "<> show err
  show (VerificationKeyDeserialisationFailed fp err)
    = "Verification key '" <> fp <> "' read failure: "<> T.unpack err
  show (DlgCertificateDeserialisationFailed fp err)
    = "Delegation certificate '" <> fp <> "' read failure: "<> T.unpack err
  show (TxDeserialisationFailed fp err)
    = "Transaction file '" <> fp <> "' read failure: "<> show err
  show (DelegationError err)
    = "Error while issuing delegation: " <> show err
  show (GenerateTxsError err)
    = "Error in GenerateTxs command: " <> show err
  show (NodeSubmitTxError err)
    = "Error in SubmitTx command: " <> show err
  show (SpendGenesisUTxOError err)
    = "Error in SpendGenesisUTxO command: " <> show err
  show (GenesisSpecError err)
    = "Error in genesis specification: " <> T.unpack err
  show (GenesisGenerationError err)
    = "Genesis generation failed in mkGenesis: " <> show err
  show (IssueUtxoError err)
    = "Error SpendUTxO command: " <> show err
  show (NoGenesisDelegationForKey key)
    = "Newly-generated genesis doesn't delegate to operational key: " <> T.unpack key
  show (ReadVerificationKeyFailure fp expt)
    = "Exception encountered while trying to read the verification key file at: " <> fp
       <> "Exception: " <> T.unpack expt
  show (ReadSigningKeyFailure fp expt)
    = "Exception encountered while trying to read the signing key file at: " <> fp
       <> "Exception: " <> T.unpack expt

instance Exception CliError

data RealPBFTError =
    IncorrectProtocolSpecified !Protocol
  | FromProtocolError !ProtocolInstantiationError
  | GenesisBenchmarkRunnerError !TxGenError
  | InvariantViolation !Text
  | TransactionTypeNotHandledYet !Text
  deriving Show

data TxGenError =
    CurrentlyCannotSendTxToRelayNode !FilePath
  -- ^ Relay nodes cannot currently be transaction recipients.
  | InsufficientFundsForRecipientTx
  -- ^ Error occurred while creating the target node address.
  | NeedMinimumThreeSigningKeyFiles ![FilePath]
  -- ^ Need at least 3 signing key files.
  | SecretKeyDeserialiseError !Text
  | SecretKeyReadError !Text
  deriving Show

-- | Perform an action that expects ProtocolInfo for Byron/PBFT,
--   with attendant configuration.
withRealPBFT
  :: Text
  -> GenesisFile
  -> RequiresNetworkMagic
  -> Maybe Double
  -> Maybe DelegationCertFile
  -> Maybe SigningKeyFile
  -> Update
  -> Protocol
  -> (RunNode ByronBlock
        => Consensus.Protocol ByronBlock
        -> ExceptT RealPBFTError IO a)
  -> ExceptT RealPBFTError IO a
withRealPBFT gHash genFile nMagic sigThresh delCertFp sKeyFp update ptcl action = do
  SomeProtocol p <- firstExceptT
                      FromProtocolError
                      $ fromProtocol
                          gHash
                          Nothing
                          Nothing
                          (Just genFile)
                          nMagic
                          sigThresh
                          delCertFp
                          sKeyFp
                          update
                          ptcl
  case p of
    proto@Consensus.ProtocolRealPBFT{} -> action proto
    _ -> left $ IncorrectProtocolSpecified ptcl

--------------------------------------------------------------------------------
-- Query local node's chain tip
--------------------------------------------------------------------------------

getLocalTip
  :: ConfigYamlFilePath
  -> GenesisFile
  -> SocketPath
  -> IO ()
getLocalTip configFp genFp sockPath = do
  nc <- parseNodeConfigurationFP $ unConfigPath configFp

  eGenHash <- runExceptT $ getGenesisHash genFp

  genHash <- case eGenHash  of
               Right gHash -> pure gHash
               Left err -> do putTextLn . toS $ show err
                              exitFailure

  frmPtclRes <- runExceptT . firstExceptT ProtocolError
                           $ fromProtocol
                               genHash
                               (ncNodeId nc)
                               (ncNumCoreNodes nc)
                               (Just genFp)
                               (ncReqNetworkMagic nc)
                               (ncPbftSignatureThresh nc)
                               Nothing
                               Nothing
                               (ncUpdate nc)
                               (ncProtocol nc)

  SomeProtocol p <- case frmPtclRes of
                        Right (SomeProtocol p) -> pure (SomeProtocol p)
                        Left err -> do putTextLn . toS $ show err
                                       exitFailure

  createNodeConnection (Proxy) p sockPath


createNodeConnection
  :: forall blk . (Condense (HeaderHash blk), RunNode blk)
  => Proxy blk
  -> Consensus.Protocol blk
  -> SocketPath
  -> IO ()
createNodeConnection proxy ptcl socketPath = do
    addr <- localSocketAddrInfo socketPath
    let ProtocolInfo{pInfoConfig} = protocolInfo ptcl
    connectTo
      (NetworkConnectTracers nullTracer nullTracer)
      (localInitiatorNetworkApplication proxy pInfoConfig)
      Nothing
      addr
    `catch` handleMuxError

handleMuxError :: MuxError -> IO ()
handleMuxError err = print err

localInitiatorNetworkApplication
  :: forall blk m peer.
     ( RunNode blk
     , Condense (HeaderHash blk)
     , IOLike m
     , MonadIO m
     , MonadTimer m
     )
  => Proxy blk
  -> Consensus.NodeConfig (BlockProtocol blk)
  -> Versions NodeToClientVersion DictVersion
              (OuroborosApplication 'InitiatorApp peer NodeToClientProtocols
                                    m LB.ByteString () Void)
localInitiatorNetworkApplication proxy pInfoConfig =
    simpleSingletonVersions
      NodeToClientV_1
      (NodeToClientVersionData { networkMagic = nodeNetworkMagic proxy pInfoConfig })
      (DictVersion nodeToClientCodecCBORTerm)

  $ OuroborosInitiatorApplication $ \_peer ptcl -> case ptcl of
      LocalTxSubmissionPtcl -> \channel ->
        runPeer
          nullTracer
          localTxSubmissionCodec
          channel
          (localTxSubmissionClientPeer localTxSubmissionClientNull)

      ChainSyncWithBlocksPtcl -> \channel ->
        runPeer
          nullTracer
          localChainSyncCodec
          channel
          (chainSyncClientPeer chainSyncClient)
 where
  localChainSyncCodec :: Codec (ChainSync (Serialised blk) (Tip blk)) DeserialiseFailure m LB.ByteString
  localChainSyncCodec = pcLocalChainSyncCodec . protocolCodecs pInfoConfig $ mostRecentNetworkProtocolVersion proxy

  localTxSubmissionCodec :: Codec (LocalTxSubmission (GenTx blk) (ApplyTxErr blk)) DeserialiseFailure m LB.ByteString
  localTxSubmissionCodec = pcLocalTxSubmissionCodec . protocolCodecs pInfoConfig $ mostRecentNetworkProtocolVersion proxy


chainSyncClient
  :: forall blk m . (Condense (HeaderHash blk), MonadIO m)
  => ChainSyncClient (Serialised blk) (Tip blk) m ()
chainSyncClient = ChainSyncClient $ pure $
  SendMsgRequestNext
    clientStNext
    (pure $ ClientStNext
              { recvMsgRollForward = \_ _ -> ChainSyncClient $ pure clientStIdle
              , recvMsgRollBackward = \_ _ -> ChainSyncClient $ pure clientStIdle
              }
    )
 where
  clientStIdle :: ClientStIdle (Serialised blk) (Tip blk) m ()
  clientStIdle =
    SendMsgRequestNext clientStNext (pure clientStNext)

  clientStNext :: ClientStNext (Serialised blk) (Tip blk) m ()
  clientStNext = ClientStNext
    { recvMsgRollForward = \_blk tip -> ChainSyncClient $ do
        traceWith stdoutTracer $ showTip MaximalVerbosity tip
        pure $ SendMsgDone ()
    , recvMsgRollBackward = \_point tip -> ChainSyncClient $ do
        traceWith stdoutTracer $ showTip MaximalVerbosity tip
        pure $ SendMsgDone ()

    }
