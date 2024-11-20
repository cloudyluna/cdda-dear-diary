{-# LANGUAGE RecordWildCards #-}

module CddaDearDiary.Internal.Parser.Entry (
    firstEntryStatsParser,
    firstEntrySkillsParser,
    firstEntryListBaseParser,
    activeMissionsParser,
) where

import CddaDearDiary.Internal.Parser.Single
import CddaDearDiary.Types
import Control.Monad (void)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L


firstEntryStatsParser :: Parser CharacterStats
firstEntryStatsParser = do
    void $ string "Stats:" <* optional spaceChar <* optional newline
    strength <- prop "Strength"
    dexterity <- prop "Dexterity"
    intelligence <- prop "Intelligence"
    perception <- prop "Perception"

    pure $ MkCharacterStats{..}


prop ::
    ( Token s ~ Char
    , Num a
    , IsString (Tokens s)
    , Semigroup (Tokens s)
    , MonadParsec e s f
    ) =>
    Tokens s ->
    f a
prop s = string (s <> ":") *> space1 *> L.decimal <* newline


firstEntrySkillsParser :: Parser CharacterSkills
firstEntrySkillsParser = do
    void $ string "Skills:" *> newline
    computers <- prop "computers"
    electronics <- prop "electronics"
    fabrication <- prop "fabrication"
    marksmanship <- prop "marksmanship"
    mechanics <- prop "mechanics"
    survival <- prop "survival"
    devices <- prop "devices"
    healthCare <- prop "health care"
    athletics <- prop "athletics"
    archery <- prop "archery"
    bashingWeapons <- prop "bashing weapons"
    cuttingWeapons <- prop "cutting weapons"
    rifles <- prop "rifles"
    piercingWeapons <- prop "piercing weapons"
    vehicles <- prop "vehicles"
    social <- prop "social"
    foodHandling <- prop "food handling"
    tailoring <- prop "tailoring"
    appliedScience <- prop "applied science"

    pure $ MkCharacterSkills{..}


firstEntryListBaseParser :: Text -> [Parser Char] -> Parser [Text]
firstEntryListBaseParser keyword additionalParsers = do
    void $ string' keyword *> colon *> newline
    manyTill propertyParser endKeyword
    where
        propertyParser =
            T.pack
                <$> manyTill
                    (choice $ try <$> baseParsers)
                    (newline *> space)
        baseParsers = [alphaNumChar, newline, spaceChar] <> additionalParsers
        endKeyword = try $ lookAhead (endKeywords *> colon)


endKeywords :: Parser Text
endKeywords =
    choice $
        string'
            <$> [ "entry"
                , "proficiencies"
                , "mutations"
                , "skills"
                , "stats"
                , "kills"
                , "active missions"
                , "new missions"
                ]


activeMissionsParser :: Parser [Text]
activeMissionsParser = do
    void $ string "Active missions:" *> newline
    manyTill propertyParser endInit
    where
        propertyParser =
            T.pack
                <$> manyTill
                    (choice $ try <$> [alphaNumChar, spaceChar])
                    newline
        endInit = spaceChar *> newline