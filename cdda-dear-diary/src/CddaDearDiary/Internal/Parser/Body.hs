module CddaDearDiary.Internal.Parser.Body (
    entryBodyParser,
    statsUpdateParser,
    skillsUpdateParser,
    proficienciesUpdateParser,
    newMissionsUpdateParser,
    gainedMutationUpdateParser,
    lostMutationUpdateParser,
    killsUpdateParser,
    npcKilledUpdateParser,
    statTransitionUpdateParser,
    allowedMissions,
) where

import CddaDearDiary.Types

import CddaDearDiary.Internal.Parser.Entry
import CddaDearDiary.Internal.Parser.Single
import Control.Monad (void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L


entryBodyParser :: Bool -> [Text] -> Parser Body
entryBodyParser isFirstEntry extendedNewMissions = do
    if isFirstEntry
        then do
            stats <- initializeWith firstEntryStatsParser
            separator
            skills <- initializeWith firstEntrySkillsParser
            separator
            proficiencies <- initializeWith $ firstEntryListBaseParser "proficiencies" []
            separator
            mutations <-
                initializeWith $ firstEntryListBaseParser "mutations" [colon, comma]
            separator
            activeMissions <- Just . Initiation <$> activeMissionsParser

            MkBody
                stats
                skills
                proficiencies
                mutations
                activeMissions
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                <$> makeNote
        else do
            -- TODO: Ensure any order with choice parser.
            stats <- optional $ try statsUpdateParser
            separator
            skills <- optional $ try skillsUpdateParser
            separator
            proficiencies <- optional $ try proficienciesUpdateParser
            separator
            newMissions <- optional $ try (newMissionsUpdateParser extendedNewMissions)
            separator
            gainedMutation <- optional $ try gainedMutationUpdateParser
            separator
            lostMutation <- optional $ try lostMutationUpdateParser
            separator
            kills <- optional $ try killsUpdateParser
            separator
            npcKilled <- optional $ try npcKilledUpdateParser
            MkBody
                (Update <$> stats)
                (Update <$> skills)
                (Update <$> proficiencies)
                Nothing
                Nothing
                (Update <$> newMissions)
                (Update <$> gainedMutation)
                (Update <$> lostMutation)
                (Update <$> kills)
                (Update <$> npcKilled)
                <$> makeNote
    where
        makeNote = try anythingUntilNextEntryKeyword <|> try anythingParser

        anythingUntilNextEntryKeyword =
            T.pack
                <$> manyTill
                    (anySingle <?> "Any char for note")
                    (lookAhead entryKeywordParser)
        separator = void $ optional newline *> optional spaceChar *> optional newline
        initializeWith = fmap (Just . Initiation)


statsUpdateParser :: Parser (Map Text StatUpdate)
statsUpdateParser = do
    void $ string "Stats:" *> spaceChar *> newline
    uxs <- many $ choice parsers
    void $ spaceChar *> newline
    pure $ M.fromList uxs
    where
        parsers = updatePropertyParser <$> keywords
        keywords = ["Strength", "Dexterity", "Intelligence", "Perception"]


skillsUpdateParser :: Parser (Map Text StatUpdate)
skillsUpdateParser = do
    void $ string "Skills:" *> spaceChar *> newline
    uxs <- many $ choice parsers
    void $ spaceChar *> newline
    pure $ M.fromList uxs
    where
        parsers = updatePropertyParser <$> keywords
        keywords =
            [ "computers"
            , "electronics"
            , "fabrication"
            , "marksmanship"
            , "mechanics"
            , "survival"
            , "devices"
            , "health care"
            , "athletics"
            , "archery"
            , "bashing weapons"
            , "cutting weapons"
            , "rifles"
            , "piercing weapons"
            , "vehicles"
            , "social"
            , "food handling"
            , "tailoring"
            , "applied science"
            , -- TODO: Not listed in initial one. Do we need this in that too?
              "melee"
            , -- TODO: Same question as above.
              "handguns"
            ]


proficienciesUpdateParser :: Parser [Text]
proficienciesUpdateParser = do
    void $ string "Proficiencies:" *> spaceChar *> newline
    manyTill propertyParser keywordEnd
    where
        propertyParser =
            T.pack
                <$> manyTill
                    (choice $ try <$> [alphaNumChar, spaceChar])
                    newline
        keywordEnd = spaceChar *> newline


{- | Cannot parse the rest of list because the end separator is the exact same
with everything from beginning, which is frustating because other properties
has different /ending/ indicator.
Workaround: We use fixed list of base missions to determine acceptable new missions.
This can be extended with the extendedNewMissions list.
-}
newMissionsUpdateParser :: [Text] -> Parser [Text]
newMissionsUpdateParser extendedNewMissions = do
    void $ string "New missions:" *> newline
    many (missionsParser <* newline <* spaceChar <* optional newline)
    where
        missionsParser = choice $ string <$> (allowedMissions ++ extendedNewMissions)


allowedMissions :: [Text]
allowedMissions =
    [ "Follow Sarcophagus Team"
    , "Find Inhaler"
    , "Find Antibiotics"
    , "Retrieve Military Black Box"
    , "Retrieve Black Box Transcript"
    , "Find Deputy Badge"
    , "Find Flag"
    , "Find Corporate Accounts"
    , "Find Patient Records"
    , "Find Weather Log"
    , "Find a Holy Symbol"
    , "Find Relic"
    , "Retrieve Deposit Box"
    , "Retrieve Software"
    , "Analyze Zombie Blood"
    , "Investigate Cult"
    , "Prison Visionary"
    , "Find Deputy Badge"
    , "Kill 100 Zombies"
    , "Kill Horde Master"
    , "Kill Jabberwock"
    , "Kill Zombie Mom"
    , "Null mission"
    , "Reach Farm House"
    , "Reach FEMA Camp"
    , "Reach Safety"
    , "Reach Refugee Center"
    , "Recover Priest's Diary"
    , "Recruit Tracker"
    , "Find Lost Dog"
    , "Break into armory to retrieve family photo"
    , "Prove your worth to Foodperson"
    , "Prove You're A Survivor"
    , "Gather cattail stalks to create cattail jelly"
    , "Angry pyromaniac"
    , "Investigate Strange Location"
    , "Center of the storm"
    , "Faction succession"
    ]


gainedMutationUpdateParser :: Parser [Text]
gainedMutationUpdateParser = do
    void $ string "Gained Mutation:" *> spaceChar *> newline
    manyTill propertyParser keywordEnd
    where
        propertyParser =
            T.pack
                <$> manyTill
                    (choice $ try <$> [alphaNumChar, spaceChar])
                    newline
        keywordEnd = spaceChar *> newline


lostMutationUpdateParser :: Parser [Text]
lostMutationUpdateParser = do
    void $ string "Lost Mutation:" *> spaceChar *> newline
    manyTill propertyParser keywordEnd
    where
        propertyParser =
            T.pack
                <$> manyTill
                    (choice $ try <$> [alphaNumChar, spaceChar])
                    newline
        keywordEnd = spaceChar *> newline


-- The first and the last entry can have (min) 2 or 3 (max) trailing spaces.
killsUpdateParser :: Parser [CharacterKills]
killsUpdateParser = do
    void $
        string "Kills:"
            *> spaceChar
            *> newline
            *> (spaceChar *> spaceChar *> optional spaceChar)
    many (killsParser <* separator)
    where
        killsParser = do
            count0 <- L.decimal <* spaceChar
            -- FIXME: Add more allowed symbols.
            symbol <-
                T.pack <$> manyTill (choice [alphaNumChar, char '@', char '&']) spaceChar
            MkCharacterKills count0 symbol
                <$> ( T.pack
                        <$> manyTill
                            -- FIXME: Can contain symbols too!
                            (choice [alphaNumChar, spaceChar, char '-'])
                            newline
                    )
        separator =
            -- TODO: Refactor once test is strong.
            try (spaceChar *> newline *> notFollowedBy spaceChar)
                <|> try (spaceChar *> spaceChar *> notFollowedBy spaceChar)
                <|> try (spaceChar *> spaceChar *> spaceChar *> notFollowedBy spaceChar)


npcKilledUpdateParser :: Parser [CharacterNPCKilled]
npcKilledUpdateParser = do
    do
        void $
            string "NPC Killed:"
                *> spaceChar
                *> newline
                *> (spaceChar *> spaceChar *> optional spaceChar)
        many (killsParser <* separator)
    where
        killsParser = do
            count0 <- L.decimal <* spaceChar
            -- FIXME: Add more allowed symbols.
            symbol <-
                T.pack <$> manyTill (choice [alphaNumChar, char '@', char '&']) spaceChar
            MkCharacterNPCKilled count0 symbol
                <$> ( T.pack
                        <$> manyTill
                            -- FIXME: Can contain symbols too!
                            (choice [alphaNumChar, spaceChar, char '-'])
                            newline
                    )
        separator =
            -- TODO: Refactor once test is strong.
            try (spaceChar *> newline *> notFollowedBy spaceChar)
                <|> try (spaceChar *> spaceChar *> notFollowedBy spaceChar)
                <|> try (spaceChar *> spaceChar *> spaceChar *> notFollowedBy spaceChar)


updatePropertyParser :: Text -> Parser (Text, StatUpdate)
updatePropertyParser keyword = do
    mSU <-
        string (keyword <> ":") *> spaceChar *> statTransitionUpdateParser <* newline
    pure (normalize keyword, mSU)
    where
        -- This list is fixed, so this is simple way to do it.
        normalize = \case
            "health care" -> "healthCare"
            "bashing weapons" -> "bashingWeapons"
            "cutting weapons" -> "cuttingWeapons"
            "piercing weapons" -> "piercingWeapons"
            "food handling" -> "foodHandling"
            "applied science" -> "appliedScience"
            _rest -> _rest


statTransitionUpdateParser :: Parser StatUpdate
statTransitionUpdateParser = do
    from <- L.decimal <* spaceChar
    void $ string "->" *> spaceChar
    MkStatUpdate from <$> L.decimal