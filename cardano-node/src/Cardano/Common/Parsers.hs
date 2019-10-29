{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Common.Parsers
  ( cliTracingParser
  , loggingParser
  , nodeMockParser
  , nodeMockProtocolModeParser
  , nodeProtocolModeParser
  , parseConfigFile
  , parseCoreNodeId
  , parseDbPath
  , parseLogConfigFileLast
  , parseLogMetricsLast
  , parseLogOutputFile
  , parseNodeId
  , parseProtocol
  , parseProtocolBFT
  , parseProtocolByron
  , parseProtocolMockPBFT
  , parseProtocolPraos
  , parseProtocolRealPBFT
  , parseTopologyInfo
  , parseTraceOptions
  ) where


import           Prelude (String)

import           Cardano.Prelude hiding (option)

import           Network.Socket (PortNumber)
import           Options.Applicative

import           Cardano.BM.Data.Tracer (TracingVerbosity (..))
import           Cardano.Config.Logging (LoggingCLIArguments(..))
import           Ouroboros.Consensus.NodeId (NodeId(..), CoreNodeId(..))

import           Cardano.Config.CommonCLI
import           Cardano.Config.Protocol
import           Cardano.Config.Topology
import           Cardano.Config.Types (ConfigYamlFilePath(..), DbFile(..),
                                       DelegationCertFile(..), GenesisFile (..),
                                       MiscellaneousFilepaths(..),
                                       NodeMockCLI(..), NodeProtocolMode (..),
                                       NodeRealCLI (..), SigningKeyFile(..),
                                       SocketFile(..), TraceOptions(..), TopologyFile(..))

-- Common command line parsers

cliTracingParser :: Parser (Last TraceOptions)
cliTracingParser = Last . Just <$> parseTraceOptions


nodeProtocolModeParser  :: Parser NodeProtocolMode
nodeProtocolModeParser = nodeMockProtocolModeParser <|> nodeRealProtocolModeParser

nodeMockProtocolModeParser :: Parser NodeProtocolMode
nodeMockProtocolModeParser = subparser
                           (  commandGroup "Execute node with a mock protocol."
                           <> metavar "mock-protocol"
                           <> command "mock-protocol"
                                (MockProtocolMode
                                  <$> info
                                        (nodeMockParser <**> helper)
                                        (progDesc "Execute node with a mock protocol."))
                           )
nodeRealProtocolModeParser :: Parser NodeProtocolMode
nodeRealProtocolModeParser = subparser
                           (  commandGroup "Execute node with a real protocol."
                           <> metavar "real-protocol"
                           <> command "real-protocol"
                                (RealProtocolMode
                                  <$> info
                                        (nodeRealParser <**> helper)
                                        (progDesc "Execute node with a real protocol." ))
                           )


-- | The mock protocol parser.
nodeMockParser :: Parser NodeMockCLI
nodeMockParser = do
  -- Filepaths
  topFp <- parseTopologyFile
  dbFp <- parseDbPath
  genFp <- parseGenesisPath
  delCertFp <- optional parseDelegationCert
  sKeyFp <- optional parseSigningKey
  socketFp <- parseSocketDir

  -- Node Address
  nAddress <- parseNodeAddress
  -- NodeConfiguration filepath
  nodeConfigFp <- parseConfigFile

  -- TraceOptions
  traceOptions <- cliTracingParser


  pure $ NodeMockCLI
           (MiscellaneousFilepaths
              (TopologyFile topFp)
              (DbFile dbFp)
              (GenesisFile genFp)
              (DelegationCertFile <$> delCertFp)
              (SigningKeyFile <$> sKeyFp)
              (SocketFile socketFp)
            )
           nAddress
           (ConfigYamlFilePath nodeConfigFp)
           (fromMaybe (panic "Cardano.Common.Parsers: Trace Options were not specified") $ getLast traceOptions)

-- | The real protocol parser.
nodeRealParser :: Parser NodeRealCLI
nodeRealParser = do
  -- Filepaths
  topFp <- parseTopologyFile
  dbFp <- parseDbPath
  genFp <- parseGenesisPath
  delCertFp <- optional parseDelegationCert
  sKeyFp <- optional parseSigningKey
  socketFp <- parseSocketPath

  -- Node Address
  nAddress <- parseNodeAddress

  -- NodeConfiguration filepath
  nodeConfigFp <- parseConfigFile

  -- TraceOptions
  traceOptions <- cliTracingParser


  pure $ NodeRealCLI
           (MiscellaneousFilepaths
              (TopologyFile topFp)
              (DbFile dbFp)
              (GenesisFile genFp)
              (DelegationCertFile <$> delCertFp)
              (SigningKeyFile <$> sKeyFp)
              (SocketFile socketFp)
            )
           nAddress
           (ConfigYamlFilePath nodeConfigFp)
           (fromMaybe (panic "Cardano.Common.Parsers: Trace Options were not specified") $ getLast traceOptions)



parseConfigFile :: Parser FilePath
parseConfigFile =
  strOption
    ( long "config"
    <> metavar "NODE-CONFIGURATION"
    <> help "Configuration file for the cardano-node"
    <> completer (bashCompleter "file")
    )

parseDbPath :: Parser FilePath
parseDbPath =
  strOption
    ( long "database-path"
    <> metavar "FILEPATH"
    <> help "Directory where the state is stored."
    )

parseCoreNodeId :: Parser CoreNodeId
parseCoreNodeId =
    option (fmap CoreNodeId auto) (
            long "core-node-id"
         <> metavar "CORE-NODE-ID"
         <> help "The ID of the core node to which this client is connected."
    )

parseNodeId :: String -> Parser NodeId
parseNodeId desc =
    option (fmap CoreId auto) (
            long "node-id"
         <> metavar "NODE-ID"
         <> help desc
    )

parseNodeAddress :: Parser NodeAddress
parseNodeAddress = NodeAddress <$> parseHostAddr <*> parsePort

parseHostAddr :: Parser NodeHostAddress
parseHostAddr =
    option (NodeHostAddress . readMaybe <$> str) (
          long "host-addr"
       <> metavar "HOST-NAME"
       <> help "Optionally limit node to one ipv6 or ipv4 address"
       <> (value $ NodeHostAddress Nothing)
    )

parsePort :: Parser PortNumber
parsePort =
    option ((fromIntegral :: Int -> PortNumber) <$> auto) (
          long "port"
       <> metavar "PORT"
       <> help "The port number"
    )

-- | Flag parser, that returns its argument on success.
flagParser :: a -> String -> String -> Parser a
flagParser val opt desc = flag' val $ mconcat [long opt, help desc]

parseProtocol :: Parser Protocol
parseProtocol = asum
  [ flagParser ByronLegacy "byron-legacy"
    "Byron/Ouroboros Classic suite of algorithms"
  , flagParser BFT "bft"
    "BFT consensus"
  , flagParser Praos "praos"
    "Praos consensus"
  , flagParser MockPBFT "mock-pbft"
    "Permissive BFT consensus with a mock ledger"
  , flagParser RealPBFT "real-pbft"
    "Permissive BFT consensus with a real ledger"
  ]

parseProtocolByron :: Parser (Last Protocol)
parseProtocolByron =
  flagParser
    (Last $ Just ByronLegacy)
    "byron-legacy"
    "Byron/Ouroboros Classic suite of algorithms"


parseProtocolBFT :: Parser (Last Protocol)
parseProtocolBFT =
  flagParser
    (Last $ Just BFT)
    "bft"
    "BFT consensus"


parseProtocolPraos :: Parser (Last Protocol)
parseProtocolPraos =
  flagParser
    (Last $ Just Praos)
    "praos"
    "Praos consensus"


parseProtocolMockPBFT :: Parser (Last Protocol)
parseProtocolMockPBFT =
  flagParser
    (Last $ Just MockPBFT)
    "mock-pbft"
    "Permissive BFT consensus with a mock ledger"


parseProtocolRealPBFT :: Parser (Last Protocol)
parseProtocolRealPBFT =
  flagParser
    (Last $ Just RealPBFT)
    "real-pbft"
    "Permissive BFT consensus with a real ledger"


parseTopologyInfo :: String -> Parser TopologyInfo
parseTopologyInfo desc = TopologyInfo <$> parseNodeId desc <*> parseTopologyFile

parseTopologyFile :: Parser FilePath
parseTopologyFile =
    strOption (
            long "topology"
         <> metavar "FILEPATH"
         <> help "The path to a file describing the topology."
    )
parseLogOutputFile :: Parser FilePath
parseLogOutputFile =
  strOption
    ( long "log-output"
    <> metavar "FILEPATH"
    <> help "Logging output file"
    <> completer (bashCompleter "file")
    )

parseLogConfigFileLast :: Parser (Last FilePath)
parseLogConfigFileLast =
  lastStrOption
    ( long "log-config"
    <> metavar "LOGCONFIG"
    <> help "Configuration file for logging"
    <> completer (bashCompleter "file")
    )

parseLogMetricsLast :: Parser (Last Bool)
parseLogMetricsLast =
 Last . Just <$> switch
   ( long "log-metrics"
   <> help "Log a number of metrics about this node"
   )

-- | A parser disables logging if --log-config is not supplied.
loggingParser :: Parser LoggingCLIArguments
loggingParser =
  fromMaybe muteLoggingCLIArguments
    <$> optional parseLoggingCLIArgumentsInternal
  where
    parseLoggingCLIArgumentsInternal :: Parser LoggingCLIArguments
    parseLoggingCLIArgumentsInternal =
      LoggingCLIArguments
        <$> (Just
             <$> strOption
              ( long "log-config"
                <> metavar "LOGCONFIG"
                <> help "Configuration file for logging"
                <> completer (bashCompleter "file")))
        <*> switch
         ( long "log-metrics"
           <> help "Log a number of metrics about this node")

    -- This is the value returned by the parser, when --log-config is omitted.
    muteLoggingCLIArguments :: LoggingCLIArguments
    muteLoggingCLIArguments =
      LoggingCLIArguments
      Nothing
      False

-- | The parser for the logging specific arguments.
parseTraceOptions :: Parser TraceOptions
parseTraceOptions = TraceOptions
  <$> parseTracingVerbosity
  <*> parseTraceChainDB
  -- Consensus Trace Options --
  <*> parseTraceChainSyncClient
  <*> parseTraceChainSyncHeaderServer
  <*> parseTraceChainSyncBlockServer
  <*> parseTraceBlockFetchDecisions
  <*> parseTraceBlockFetchClient
  <*> parseTraceBlockFetchServer
  <*> parseTraceTxInbound
  <*> parseTraceTxOutbound
  <*> parseTraceLocalTxSubmissionServer
  <*> parseTraceMempool
  <*> parseTraceForge
  -- Protocol Tracing Options --
  <*> parseTraceChainSyncProtocol
  <*> parseTraceBlockFetchProtocol
  <*> parseTraceTxSubmissionProtocol
  <*> parseTraceLocalChainSyncProtocol
  <*> parseTraceLocalTxSubmissionProtocol
  ------------------------------
  <*> parseTraceIpSubscription
  <*> parseTraceDnsSubscription
  <*> parseTraceDnsResolver
  <*> parseTraceErrorPolicy
  <*> parseTraceMux

parseTraceBlockFetchClient :: Parser Bool
parseTraceBlockFetchClient  =
    switch (
         long "trace-block-fetch-client"
      <> help "Trace BlockFetch client."

    )

parseTraceBlockFetchServer :: Parser Bool
parseTraceBlockFetchServer =
    switch (
         long "trace-block-fetch-server"
      <> help "Trace BlockFetch server."

    )

parseTracingVerbosity :: Parser TracingVerbosity
parseTracingVerbosity =
  flag' MinimalVerbosity (
      long "tracing-verbosity-minimal"
        <> help "Minimal level of the rendering of captured items"
        )
    <|>
  flag' MaximalVerbosity (
      long "tracing-verbosity-maximal"
        <> help "Maximal level of the rendering of captured items"
        )
    <|>
  flag NormalVerbosity NormalVerbosity (
      long "tracing-verbosity-normal"
        <> help "the default level of the rendering of captured items"
        )


parseTraceChainDB :: Parser Bool
parseTraceChainDB =
    switch (
         long "trace-chain-db"
      <> help "Verbose tracer of ChainDB."

    )

parseTraceBlockFetchDecisions :: Parser Bool
parseTraceBlockFetchDecisions =
    switch (
         long "trace-block-fetch-decisions"
      <> help "Trace BlockFetch decisions made by the BlockFetch client."

    )

parseTraceChainSyncClient :: Parser Bool
parseTraceChainSyncClient =
    switch (
         long "trace-chain-sync-client"
      <> help "Trace ChainSync client."

    )

parseTraceChainSyncBlockServer :: Parser Bool
parseTraceChainSyncBlockServer =
    switch (
         long "trace-chain-sync-block-server"
      <> help "Trace ChainSync server (blocks)."

    )

parseTraceChainSyncHeaderServer :: Parser Bool
parseTraceChainSyncHeaderServer =
    switch (
         long "trace-chain-sync-header-server"
      <> help "Trace ChainSync server (headers)."

    )

parseTraceTxInbound :: Parser Bool
parseTraceTxInbound =
    switch (
         long "trace-tx-inbound"
      <> help "Trace TxSubmission server (inbound transactions)."

    )

parseTraceTxOutbound :: Parser Bool
parseTraceTxOutbound =
    switch (
         long "trace-tx-outbound"
      <> help "Trace TxSubmission client (outbound transactions)."

    )

parseTraceLocalTxSubmissionServer :: Parser Bool
parseTraceLocalTxSubmissionServer =
    switch (
         long "trace-local-tx-submission-server"
      <> help "Trace local TxSubmission server."

    )

parseTraceMempool :: Parser Bool
parseTraceMempool =
    switch (
         long "trace-mempool"
      <> help "Trace mempool."

    )

parseTraceForge :: Parser Bool
parseTraceForge =
    switch (
         long "trace-forge"
      <> help "Trace block forging."

    )

parseTraceChainSyncProtocol :: Parser Bool
parseTraceChainSyncProtocol =
    switch (
         long "trace-chain-sync-protocol"
      <> help "Trace ChainSync protocol messages."

    )

parseTraceBlockFetchProtocol :: Parser Bool
parseTraceBlockFetchProtocol =
    switch (
         long "trace-block-fetch-protocol"
      <> help "Trace BlockFetch protocol messages."

    )

parseTraceTxSubmissionProtocol :: Parser Bool
parseTraceTxSubmissionProtocol =
    switch (
         long "trace-tx-submission-protocol"
      <> help "Trace TxSubmission protocol messages."

    )

parseTraceLocalChainSyncProtocol :: Parser Bool
parseTraceLocalChainSyncProtocol =
    switch (
         long "trace-local-chain-sync-protocol"
      <> help "Trace local ChainSync protocol messages."

    )

parseTraceLocalTxSubmissionProtocol :: Parser Bool
parseTraceLocalTxSubmissionProtocol =
    switch (
         long "trace-local-tx-submission-protocol"
      <> help "Trace local TxSubmission protocol messages."

    )


parseTraceIpSubscription :: Parser Bool
parseTraceIpSubscription =
    switch (
         long "trace-ip-subscription"
      <> help "Trace IP Subscription messages."

    )

parseTraceDnsSubscription :: Parser Bool
parseTraceDnsSubscription =
    switch (
         long "trace-dns-subscription"
      <> help "Trace DNS Subscription messages."

    )

parseTraceDnsResolver :: Parser Bool
parseTraceDnsResolver =
    switch (
         long "trace-dns-resolver"
      <> help "Trace DNS Resolver messages."

    )

parseTraceErrorPolicy :: Parser Bool
parseTraceErrorPolicy =
    switch (
         long "trace-error-policy"
      <> help "Trace error policy resolution."
    )

parseTraceMux :: Parser Bool
parseTraceMux =
    switch (
         long "trace-mux"
      <> help "Trace Mux Events"

    )
