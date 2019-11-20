{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import           Cardano.Prelude hiding (option)
import           Prelude (String)

import           Data.Semigroup ((<>))
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

import           Cardano.Shell.Lib (runCardanoApplicationWithFeatures)
import           Cardano.Shell.Types (CardanoApplication (..),
                                      CardanoFeature (..),)

import           Cardano.Common.Help
import           Cardano.Common.Parsers
import           Cardano.Config.CommonCLI (parseCommonCLIAdvanced)
import           Cardano.Config.Logging (createLoggingFeature)
import           Cardano.Config.Types (CardanoEnvironment (..), ConfigYamlFilePath(..),
                                       NodeMockCLI(..), NodeProtocolMode (..),
                                       NodeRealCLI(..), parseNodeConfiguration)
import           Cardano.Node.Features.Node

main :: IO ()
main = do
    cli <- Opt.customExecParser p opts

    (features, nodeLayer) <- initializeAllFeatures cli env

    runCardanoApplicationWithFeatures features (cardanoApplication nodeLayer)

    where
      p = Opt.prefs Opt.showHelpOnEmpty

      env :: CardanoEnvironment
      env = NoEnvironment

      cardanoApplication :: NodeLayer -> CardanoApplication
      cardanoApplication = CardanoApplication . nlRunNode

      opts :: Opt.ParserInfo NodeProtocolMode
      opts =
        Opt.info (nodeProtocolModeParser
                    <**> helperBrief "help-tracing" "Show help for tracing options" cliHelpTracing
                    <**> helperBrief "help-advanced" "Show help for advanced options" cliHelpAdvanced
                 )

          ( Opt.fullDesc <>
            Opt.progDesc "Start node of the Cardano blockchain."
          )

      helperBrief :: String -> String -> String -> Parser (a -> a)
      helperBrief l d helpText = Opt.abortOption (Opt.InfoMsg helpText) $ mconcat
        [ Opt.long l
        , Opt.help d ]

      cliHelpTracing :: String
      cliHelpTracing = renderHelpDoc 80 $
        "Additional tracing options:"
        <$$> ""
        <$$> parserHelpOptions cliTracingParser

      cliHelpAdvanced :: String
      cliHelpAdvanced = renderHelpDoc 80 $
        "Advanced options:"
        <$$> ""
        <$$> parserHelpOptions parseCommonCLIAdvanced


initializeAllFeatures
  :: NodeProtocolMode
  -> CardanoEnvironment
  -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures (RealProtocolMode (rnCli@(NodeRealCLI _  ncFp _))) cardanoEnvironment = do
  (loggingLayer, loggingFeature) <- createLoggingFeature cardanoEnvironment rnCli

  nodeConfig <- parseNodeConfiguration $ unConfigPath ncFp
  (nodeLayer   , nodeFeature)    <-
    createNodeFeature
      loggingLayer
      cardanoEnvironment
      nodeConfig
      rnCli

  pure ([ loggingFeature
        , nodeFeature
        ] :: [CardanoFeature]
       , nodeLayer)

initializeAllFeatures (MockProtocolMode (mnCli@(NodeMockCLI _ _ ncFp _)))
                       cardanoEnvironment = do

    (loggingLayer, loggingFeature) <- createLoggingFeature cardanoEnvironment mnCli

    nodeConfig <- parseNodeConfiguration $ unConfigPath ncFp
    (nodeLayer   , nodeFeature)    <-
      createNodeFeature
        loggingLayer
        cardanoEnvironment
        nodeConfig
        mnCli

    pure ([ loggingFeature
          , nodeFeature
          ] :: [CardanoFeature]
         , nodeLayer)
