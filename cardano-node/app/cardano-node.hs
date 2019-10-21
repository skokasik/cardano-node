{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import           Cardano.Prelude hiding (option)
import           Prelude (read)

import qualified Data.IP as IP
import           Data.Semigroup ((<>))
import           Network.Socket (PortNumber)
import           Options.Applicative ( Parser, auto, flag, help, helper,
                                       long, metavar, option, str, value)
import qualified Options.Applicative as Opt

import           Cardano.Shell.Lib (runCardanoApplicationWithFeatures)
import           Cardano.Shell.Types (CardanoApplication (..),
                                      CardanoFeature (..),)
import qualified Ouroboros.Consensus.BlockchainTime as Consensus

import           Cardano.Common.Parsers
import           Cardano.Config.Protocol
import           Cardano.Config.CommonCLI
import           Cardano.Config.Logging (createLoggingFeature)
import           Cardano.Config.Partial (PartialCardanoConfiguration (..),
                                         PartialCore (..), PartialNode (..),
                                         mkCardanoConfiguration)
import           Cardano.Config.Presets (mainnetConfiguration)
import           Cardano.Config.Types (CardanoEnvironment (..), RequireNetworkMagic)
import           Cardano.Config.Topology (NodeAddress (..), TopologyInfo)
import           Cardano.Node.Features.Node
import           Cardano.Node.Run
import           Cardano.Tracing.Tracers

main :: IO ()
main = do
    cli <- Opt.customExecParser p opts

    (features, nodeLayer) <- initializeAllFeatures cli pcc env

    runCardanoApplicationWithFeatures features (cardanoApplication nodeLayer)

    where
      p :: Opt.ParserPrefs
      p = Opt.prefs Opt.showHelpOnEmpty

      pcc :: PartialCardanoConfiguration
      pcc = mainnetConfiguration

      env :: CardanoEnvironment
      env = NoEnvironment

      cardanoApplication :: NodeLayer -> CardanoApplication
      cardanoApplication = CardanoApplication . nlRunNode

      opts :: Opt.ParserInfo NodeCLI
      opts =
        Opt.info (nodeCliParser <**> helper)
          ( Opt.fullDesc <>
            Opt.progDesc "Start node of the Cardano blockchain."
          )


initializeAllFeatures
  :: NodeCLI
  -> PartialCardanoConfiguration
  -> CardanoEnvironment
  -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures (NodeCLI parsedPcc)
                      partialConfigPreset cardanoEnvironment = do

    -- `partialConfigPreset` and `parsedPcc` are merged then checked here using
    -- the partial options monoid approach.
    -- https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67
    finalConfig <- case mkCardanoConfiguration $ partialConfigPreset <> parsedPcc of
                     Left e -> throwIO e
                     Right x -> pure x

    (loggingLayer, loggingFeature) <- createLoggingFeature cardanoEnvironment finalConfig
    (nodeLayer   , nodeFeature)    <-
      createNodeFeature
        loggingLayer
        cardanoEnvironment
        finalConfig

    pure ([ loggingFeature
          , nodeFeature
          ] :: [CardanoFeature]
         , nodeLayer)

-------------------------------------------------------------------------------
-- Parsers & Types
-------------------------------------------------------------------------------

data NodeCLI = NodeCLI !PartialCardanoConfiguration

-- | The product parser for all the CLI arguments.
nodeCliParser :: Parser NodeCLI
nodeCliParser = do
  ptcl <- ( parseProtocolBFT
          <|> parseProtocolByron
          <|> parseProtocolMockPBFT
          <|> parseProtocolPraos
          <|> parseProtocolRealPBFT
          )
  topInfo <- lastOption $ parseTopologyInfo "PBFT node ID to assume."
  nAddr <- parseNodeAddress
  vMode <- parseViewMode
  logConfigFp <- parseLogConfigFile
  logMetrics <- parseLogMetrics
  dbPath <- parseDbPath
  genPath <- parseGenesisPath
  genHash <- parseGenesisHash
  delCert <- parseDelegationeCert
  sKey <- parseSigningKey
  socketDir <- parseSocketDir
  traceOptions <- cliTracingParser
  pbftSigThresh <- parsePbftSigThreshold
  reqNetMagic <- parseRequireNetworkMagic
  slotLength <- parseSlotLength

  pure $ NodeCLI
         (createPcc dbPath socketDir topInfo nAddr ptcl logConfigFp vMode
                    logMetrics genPath genHash delCert sKey pbftSigThresh
                    reqNetMagic traceOptions slotLength)
 where
  -- This merges the command line parsed values into one `PartialCardanoconfiguration`.
  createPcc
    :: Last FilePath
    -> Last FilePath
    -> Last TopologyInfo
    -> Last NodeAddress
    -> Last Protocol
    -> Last FilePath
    -> Last ViewMode
    -> Last Bool
    -> Last FilePath
    -> Last Text
    -> Last FilePath
    -> Last FilePath
    -> Last Double
    -> Last RequireNetworkMagic
    -> Last TraceOptions
    -> Last Consensus.SlotLength
    -> PartialCardanoConfiguration
  createPcc
    dbPath
    socketDir
    topInfo
    nAddr
    ptcl
    logConfigFp
    vMode
    logMetrics
    genPath
    genHash
    delCert
    sKey
    pbftSigThresh
    reqNetMagic
    traceOptions
    slotLength = mempty { pccDBPath = dbPath
                        , pccNodeAddress = nAddr
                        , pccSocketDir = socketDir
                        , pccTopologyInfo = topInfo
                        , pccProtocol = ptcl
                        , pccViewMode = vMode
                        , pccLogConfig = logConfigFp
                        , pccLogMetrics = logMetrics
                        , pccCore = mempty { pcoGenesisFile = genPath
                                           , pcoGenesisHash = genHash
                                           , pcoStaticKeyDlgCertFile = delCert
                                           , pcoStaticKeySigningKeyFile = sKey
                                           , pcoPBftSigThd = pbftSigThresh
                                           , pcoRequiresNetworkMagic = reqNetMagic
                                           }
                        , pccNode = mempty { pnoSlotLength = slotLength }
                        , pccTraceOptions = traceOptions
                        }



cliTracingParser :: Parser (Last TraceOptions)
cliTracingParser = Last . Just <$> parseTraceOptions Opt.hidden

parseNodeAddress :: Parser (Last NodeAddress)
parseNodeAddress = lastOption $ NodeAddress <$> parseHostAddr <*> parsePort

parseHostAddr :: Parser (Maybe IP.IP)
parseHostAddr =
    option (Just <$> read <$> str) (
          long "host-addr"
       <> metavar "HOST-NAME"
       <> help "Optionally limit node to one ipv6 or ipv4 address"
       <> value Nothing
    )

parsePort :: Parser PortNumber
parsePort =
    option ((fromIntegral :: Int -> PortNumber) <$> auto) (
          long "port"
       <> metavar "PORT"
       <> help "The port number"
    )

-- Optional flag for live view (with TUI graphics).
parseViewMode :: Parser (Last ViewMode)
parseViewMode =
    flag (Last $ Just SimpleView) (Last $ Just LiveView) $ mconcat
        [ long "live-view"
        , help "Live view with TUI."
        ]
