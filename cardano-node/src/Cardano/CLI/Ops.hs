{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Ops
  ( decodeCBOR
  , deserialiseDelegateKey
  , pprintCBOR
  , pprintCBORasJSON
  , readCBOR
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

import qualified Prelude as Prelude
import           Cardano.Prelude hiding (option)

import           Codec.CBOR.Pretty (prettyHexEnc)
import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, handleIOExceptT, hoistEither, left)
import           Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Text as T
import qualified Text.JSON.Canonical as CanonicalJSON
import           Test.Cardano.Prelude (canonicalEncodePretty)

import           Cardano.Binary ( Annotated(..), ByteSpan(..), Decoder
                                , DecoderError, ToCBOR, decodeFullDecoder
                                , fromCBOR, toCBOR)
import           Cardano.Crypto (RequiresNetworkMagic, SigningKey (..))
import qualified Cardano.Crypto.Signing as Crypto
import           Cardano.Crypto.ProtocolMagic (ProtocolMagicId(..))
import           Cardano.Chain.Block
                   (abobHdrFromBlock, aBlockOrBoundaryHdr
                   , aHeaderProtocolMagicId, fromCBORABlockOrBoundary
                   , toCBORABlockOrBoundary)
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochSlots(..))
import           Cardano.Chain.Update (Proposal)
import qualified Cardano.Chain.UTxO as UTxO
import           Ouroboros.Consensus.Ledger.Byron (ByronBlock)
import           Ouroboros.Consensus.Node.Run (RunNode)
import qualified Ouroboros.Consensus.Protocol as Consensus

import           Cardano.Config.Protocol
                   (Protocol(..), ProtocolInstantiationError
                   , SomeProtocol(..), fromProtocol)
import           Cardano.Config.Types
import qualified Cardano.CLI.Legacy.Byron as Legacy

decodeCBOR
  :: CBORObject
  -> LByteString
  -> (forall s. Decoder s a)
  -> ExceptT CliError IO a
decodeCBOR cborObject bs decoder = do
  case cborObject of
      CBORBlockByron -> toExceptT $ decodeFullDecoder "Byron Block" decoder bs
      CBORDelegationCertificateByron -> toExceptT $ decodeFullDecoder "Byron Delegation Certificate" decoder bs
      CBORTxByron -> toExceptT $ decodeFullDecoder "Byron Tx" decoder bs
      CBORUpdateProposalByron -> toExceptT $ decodeFullDecoder "Byron Update Proposal" decoder bs
 where
   toExceptT :: Either DecoderError a -> ExceptT CliError IO a
   toExceptT = firstExceptT CBORDecodingError . hoistEither

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

pprintCBOR :: CBORObject -> LByteString -> ExceptT CliError IO ()
pprintCBOR cborObject bs =
  case cborObject of
    CBORBlockByron -> do
      val <- decodeCBOR CBORBlockByron bs (fromCBORABlockOrBoundary $ EpochSlots 21600)
      let blockHeader = abobHdrFromBlock val
      -- Epoch boundary blocks do not have a `ProtocolMagicId`
      -- and therefore when CBOR serializing an epoch boundary block
      -- the value is not used. TODO: Modify `toCBORABlockOrBoundary`
      -- to take a `Maybe ProtocolMagicId` instead.
      let dummyPmId = Annotated (ProtocolMagicId 9999999) (ByteSpan 99 99)
      let aPmid = aBlockOrBoundaryHdr aHeaderProtocolMagicId (const dummyPmId) blockHeader
      let cborblock = toCBORABlockOrBoundary (unAnnotated aPmid) (EpochSlots 21600) val
      liftIO . putStrLn $ prettyHexEnc cborblock
    CBORDelegationCertificateByron ->
      decodeCBOR CBORDelegationCertificateByron bs (fromCBOR :: Decoder s Delegation.Certificate) >>= pprint
    CBORTxByron ->
      decodeCBOR CBORTxByron bs (fromCBOR :: Decoder s UTxO.Tx) >>= pprint
    CBORUpdateProposalByron ->
      decodeCBOR CBORTxByron bs (fromCBOR :: Decoder s Proposal) >>= pprint
 where
  pprint :: ToCBOR a => a -> ExceptT CliError IO ()
  pprint x = liftIO . putStrLn . prettyHexEnc $ toCBOR x

pprintCBORasJSON :: CBORObject -> LByteString -> ExceptT CliError IO ()
pprintCBORasJSON cborObject bs =
  case cborObject of
    CBORBlockByron ->
      decodeCBOR CBORBlockByron bs (fromCBORABlockOrBoundary $ EpochSlots 21600) >>= pprint
    CBORDelegationCertificateByron ->
      decodeCBOR CBORDelegationCertificateByron bs (fromCBOR :: Decoder s Delegation.Certificate) >>= pprint
    CBORTxByron ->
      decodeCBOR CBORTxByron bs (fromCBOR :: Decoder s UTxO.Tx) >>= pprint
    CBORUpdateProposalByron ->
      decodeCBOR CBORTxByron bs (fromCBOR :: Decoder s Proposal) >>= pprint
 where
  pprint :: ToJSON a => a -> ExceptT CliError IO ()
  pprint x = liftIO . putStrLn . LBC.unpack $ encode x

readCBOR :: FilePath -> ExceptT CliError IO LByteString
readCBOR fp =
  handleIOExceptT
    (ReadCBORFileFailure fp . toS . displayException)
    (LB.readFile fp)

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
  -- Basic user errors
  = OutputMustNotAlreadyExist !FilePath
  | ProtocolNotSupported !Protocol
  | NotEnoughTxInputs
  | NotEnoughTxOutputs
  -- Validation errors
  | CertificateValidationErrors !FilePath ![Text]
  -- Serialization errors
  | ProtocolParametersParseFailed !FilePath !Text
  | GenesisReadError !FilePath !Genesis.GenesisDataError
  | SigningKeyDeserialisationFailed !FilePath !DeserialiseFailure
  | VerificationKeyDeserialisationFailed !FilePath !Text
  | DlgCertificateDeserialisationFailed !FilePath !Text
  | TxDeserialisationFailed !FilePath !DecoderError
  -- TODO:  sadly, VerificationKeyParseError isn't exported from Cardano.Crypto.Signing/*
  -- Inconsistencies
  | CBORDecodingError DecoderError
  | DelegationError !Genesis.GenesisDelegationError
  | GenesisSpecError !Text
  | GenerateTxsError !RealPBFTError
  | GenesisGenerationError !Genesis.GenesisDataGenerationError
  | IssueUtxoError !RealPBFTError
  | NodeSubmitTxError !RealPBFTError
  -- Invariants/assertions -- does it belong here?
  | NoGenesisDelegationForKey !Text
  -- File reading errors
  | ReadCBORFileFailure !FilePath !Text
  | ReadVerificationKeyFailure !FilePath !Text
  -- ^ An exception was encountered while trying to read
  -- the verification key file.
  | ReadSigningKeyFailure !FilePath !Text
  | SpendGenesisUTxOError !RealPBFTError

instance Show CliError where
  show (CBORDecodingError e)
    = "Error with CBOR decoding: " <> show e
  show (CertificateValidationErrors fp errs)
    = Prelude.unlines $
      "Errors while validating certificate '" <> fp <> "':":
      (("  " <>) . T.unpack <$> errs)
  show (DelegationError err)
    = "Error while issuing delegation: " <> show err
  show (DlgCertificateDeserialisationFailed fp err)
    = "Delegation certificate '" <> fp <> "' read failure: "<> T.unpack err
  show (GenerateTxsError err)
    = "Error in GenerateTxs command: " <> (T.unpack $ show err)
  show (GenesisGenerationError err)
    = "Genesis generation failed in mkGenesis: " <> show err
  show (GenesisReadError fp err)
    = "Genesis file '" <> fp <> "' read failure: "<> show err
  show (GenesisSpecError err)
    = "Error in genesis specification: " <> T.unpack err
  show (IssueUtxoError err)
    = "Error SpendUTxO command: " <> (T.unpack $ show err)
  show (NodeSubmitTxError err)
    = "Error in SubmitTx command: " <> (T.unpack $ show err)
  show (NoGenesisDelegationForKey key)
    = "Newly-generated genesis doesn't delegate to operational key: " <> T.unpack key
  show NotEnoughTxInputs
    = "Transactions must have at least one input."
  show NotEnoughTxOutputs
    = "Transactions must have at least one output."
  show (OutputMustNotAlreadyExist fp)
    = "Output file/directory must not already exist: " <> fp
  show (ProtocolNotSupported proto)
    = "Unsupported protocol "<> show proto
  show (ProtocolParametersParseFailed fp err)
    = "Protocol parameters file '" <> fp <> "' read failure: "<> T.unpack err
  show (ReadSigningKeyFailure fp expt)
    = "Exception encountered while trying to read the signing key file at: " <> fp
       <> "Exception: " <> T.unpack expt
  show (ReadCBORFileFailure fp expt)
    = "Exception encountered while trying to read the CBOR file at: " <> fp
       <> "Exception: " <> T.unpack expt
  show (ReadVerificationKeyFailure fp expt)
    = "Exception encountered while trying to read the verification key file at: " <> fp
       <> "Exception: " <> T.unpack expt
  show (SigningKeyDeserialisationFailed fp err)
    = "Signing key '" <> fp <> "' read failure: "<> show err
  show (SpendGenesisUTxOError err)
    = "Error in SpendGenesisUTxO command: " <> (T.unpack $ show err)
  show (TxDeserialisationFailed fp err)
    = "Transaction file '" <> fp <> "' read failure: "<> show err
  show (VerificationKeyDeserialisationFailed fp err)
    = "Verification key '" <> fp <> "' read failure: "<> T.unpack err



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
