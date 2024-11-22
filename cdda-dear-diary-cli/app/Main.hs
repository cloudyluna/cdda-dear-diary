{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CddaDearDiary hiding (Parser)
import CddaDearDiary.Aeson ()
import Control.Monad (when)
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TL
import Data.Text.Lazy.IO qualified as TLIO
import Options.Applicative (
    Parser,
    ReadM,
    argument,
    customExecParser,
    eitherReader,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    prefs,
    progDesc,
    short,
    showHelpOnEmpty,
    str,
    switch,
    value,
    (<**>),
 )
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr)


main :: IO ()
main = handleCliOptions =<< customExecParser cliPrefs cliOpts
    where
        cliOpts =
            info
                (cliOptions <**> helper)
                (fullDesc <> progDesc "Convert CDDA's diary file to JSON format")
        cliPrefs = prefs showHelpOnEmpty


handleCliOptions :: CliOptions -> IO ()
handleCliOptions options@(MkCliOptions filename outputFilename _ _) = do
    when (null filename) $ do
        TLIO.hPutStrLn stderr "Diary file input cannot be an empty string"
        exitWith $ ExitFailure 1

    source <- tryParse options <$> T.readFile filename
    case outputFilename of
        (_ : _) -> TLIO.writeFile outputFilename source
        _empty -> TLIO.putStr source


data CliOptions = MkCliOptions
    { filename :: FilePath
    , outputFile :: FilePath
    , isCompact :: Bool
    , extendedNewMissions :: [Text]
    }
    deriving stock (Show)


cliOptions :: Parser CliOptions
cliOptions =
    MkCliOptions
        <$> argument str (metavar "DIARY_FILENAME" <> help "Diary file location")
        <*> option
            str
            ( value ""
                <> short 'o'
                <> long "output-file"
                <> help
                    "JSON output file location. This will write to the output file instead of printing to STDOUT."
            )
        <*> switch
            ( short 'c'
                <> long "compact"
                <> help "Output JSON in compact format"
            )
        <*> option
            listArgsParser
            ( value []
                <> short 'm'
                <> long "extended-new-missions"
                <> help
                    "Extra new missions to support. Example: \"Find a cat,Hug A Star,Pat the dog\""
            )
    where
        listArgsParser :: ReadM [Text]
        listArgsParser = eitherReader (Right . T.splitOn "," . T.pack)


tryParse :: CliOptions -> Text -> TL.Text
tryParse (MkCliOptions filename _ isCompact missions) source =
    case parseDiary missions filename source of
        Left err -> showParseError err
        Right parsedCode -> showJSON parsedCode
    where
        showJSON =
            if isCompact
                then encodeToLazyText
                else TL.toLazyText . encodePrettyToTextBuilder