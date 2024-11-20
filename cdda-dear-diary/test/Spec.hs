{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module Main (main) where

import CddaDearDiary (parseDiary)
import CddaDearDiary.Internal
import CddaDearDiary.Internal.Parser.Body
import CddaDearDiary.Internal.Parser.Entry
import CddaDearDiary.Internal.Parser.Single
import CddaDearDiary.Types
import Control.Monad.State.Strict (execStateT)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Expectations qualified as E
import System.FilePath (replaceExtension, takeBaseName, (</>))
import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Test.Tasty.Hspec (testSpecs)
import Text.Megaparsec (
  Parsec,
  ShowErrorComponent,
  TraversableStream,
  VisualStream,
  eof,
  parse,
 )


main :: IO ()
main =
  do
    specTests <- concat <$> traverse testSpecs [parserSpecTests]
    goldenTests' <- goldenTests
    defaultMain $
      testGroup
        "All Tests"
        [ testGroup "Specs" specTests
        , goldenTests'
        ]


parseFile :: FilePath -> IO LBS.ByteString
parseFile filename = do
  source <- T.pack <$> readFile filename
  pure . toLBS $ parseDiary [] filename source
  where
    toLBS = TL.encodeUtf8 . TL.pack <$> show


goldenTests :: IO TestTree
goldenTests = do
  diaryFiles <- findByExtension [".diary"] ("test" </> "diaries")
  pure $
    testGroup
      "Golden tests"
      [ goldenVsString (takeBaseName diaryFile) goldenFile (parseFile diaryFile)
      | diaryFile <- diaryFiles
      , let goldenFile = replaceExtension diaryFile ".golden"
      ]


-- | Simple utility function to test parsers.
(~>) ::
  ( ShowErrorComponent e
  , VisualStream input
  , TraversableStream input
  , Show expectation
  , Eq expectation
  ) =>
  Parsec e input expectation ->
  (input, expectation) ->
  Expectation
parser ~> (input, expectation) = parse parser "" input `shouldParse` expectation


(~>>) ::
  (ShowErrorComponent e, Show expectation, Eq expectation) =>
  (Text, Parsec e Text expectation) ->
  (Text, expectation) ->
  SpecWith ()
(name :: Text, parser) ~>> (input :: Text, expectation) =
  it (T.unpack name) $
    (parser <* eof) ~> (input, expectation)


(~>^) ::
  (ShowErrorComponent e, Show expectation, Eq expectation) =>
  (Text, Parsec e Text expectation) ->
  (Text, expectation) ->
  SpecWith ()
(name :: Text, parser) ~>^ (input :: Text, expectation) =
  it (T.unpack name) $ parser ~> (input, expectation)


parserSpecTests :: Spec
parserSpecTests = describe "Parser spec tests" $ do
  ("entryKeywordParser", entryKeywordParser) ~>> ("Entry:", "Entry:")
  ("entryIndexParser", entryIndexParser) ~>> ("1/3", (1, 3))
  ("yearParser", yearParser) ~>> ("Year 921", 921)
  ("seasonParser", seasonParser) ~>> ("Spring", Spring)
  ("timeParser", timeParser) ~>> ("14:02:99", MkTime 14 2 99)
  ("dateTimeParser", dateTimeParser Summer 1)
    ~>> E.dateTime
  ("lastWrittenTimeParser", lastWrittenTimeParser)
    ~>> ("1 hour, 52 minutes since last entry", MkLastWrittenTime Nothing (Just 1) 52)
  ("lastWrittenTimerParser2", lastWrittenTimeParser)
    ~>> ( "1 day, 1 hour, 52 minute since last entry"
        , MkLastWrittenTime (Just 1) (Just 1) 52
        )
  ("entryPropertiesParser", entryPropertiesParser) ~>> E.entryProperties
  ("wholeEntryParser", execStateT (wholeEntryParser []) defEntry) ~>> E.wholeEntry
  ("wholeEntryParser2", execStateT (wholeEntryParser []) defEntry)
    ~>> E.wholeEntry2
  ("entriesParser", entriesParser []) ~>> E.entries
  ( "firstEntryListBaseParser"
    , firstEntryListBaseParser "proficiencies" []
    )
    ~>^ ( "Proficiencies:\nLockpicking\nWound Care\nMutations:"
        , ["Lockpicking", "Wound Care"]
        )
  ( "firstEntryListBaseParser2"
    , firstEntryListBaseParser "mutations" [comma, colon]
    )
    ~>^ ( "Mutations:\nFRUIT\nBANANA QUEUE\nEye color: gray\nHair: brown, ponytail\nNew missions:"
        , ["FRUIT", "BANANA QUEUE", "Eye color: gray", "Hair: brown, ponytail"]
        )
  ("statTransitionUpdateParser", statTransitionUpdateParser)
    ~>> ("1 -> 9", MkStatUpdate{from = 1, to = 9})

  ("statsUpdateParser", statsUpdateParser)
    ~>> ( "Stats: \nStrength: 14 -> 25\n \n"
        , M.fromList [("Strength", MkStatUpdate{from = 14, to = 25})]
        )

  ("skillsUpdateParser", skillsUpdateParser)
    ~>> ( "Skills: \ncomputers: 1 -> 0\nelectronics: 1 -> 0\n \n"
        , M.fromList
            [ ("computers", MkStatUpdate{from = 1, to = 0})
            , ("electronics", MkStatUpdate{from = 1, to = 0})
            ]
        )

  ("skillsUpdateParser2", skillsUpdateParser)
    ~>> ( "Skills: \ncomputers: 1 -> 0\nfabrication: 2 -> 3\nfood handling: 2 -> 3\n \n"
        , M.fromList
            [ ("computers", MkStatUpdate{from = 1, to = 0})
            , ("fabrication", MkStatUpdate{from = 2, to = 3})
            , ("foodHandling", MkStatUpdate{from = 2, to = 3})
            ]
        )
  ("activeMissionsParser", activeMissionsParser)
    ~>> ( "Active missions:\nFind costplay suits\nSearch a fruit\n \n"
        , ["Find costplay suits", "Search a fruit"]
        )

  ( "newMissionsUpdateParser"
    , newMissionsUpdateParser ["Shoot A Star", "Pats A Cat"]
    )
    ~>> ( "New missions:\nFind Relic\n \nKill Jabberwock\n \nPats A Cat\n \nFind Inhaler\n \nFind Antibiotics\n \nShoot A Star\n \n"
        ,
          [ "Find Relic"
          , "Kill Jabberwock"
          , "Pats A Cat"
          , "Find Inhaler"
          , "Find Antibiotics"
          , "Shoot A Star"
          ]
        )

  ("killsUpdateParser", killsUpdateParser)
    ~>> ( "Kills: \n   9 O slimes\n   1 O big slime\n  30 o small slimes\n \n"
        ,
          [ MkCharacterKills 9 "O" "slimes"
          , MkCharacterKills 1 "O" "big slime"
          , MkCharacterKills 30 "o" "small slimes"
          ]
        )
