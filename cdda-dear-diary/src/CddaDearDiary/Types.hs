{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Module: CddaDearDiary.Types
Copyright: (c) 2024 cloudyluna
SPDX-License-Identifier: BSD-3-Clause
Maintainer: cloudyluna
-}
module CddaDearDiary.Types where

import Control.Monad.State.Strict (StateT)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec


type Parser = Parsec Void Text


data Season = Spring | Summer | Fall | Winter deriving stock (Show, Eq, Generic)


type Year = Int
type Day = Int
type Hour = Int
type Minute = Int
type Second = Int


data Time = MkTime {hour :: Hour, minute :: Minute, second :: Second}
    deriving stock (Show, Eq, Generic)


defTime :: Time
defTime = MkTime 0 0 0


data LastWrittenTime = MkLastWrittenTime {day :: Maybe Day, hour :: Maybe Hour, minute :: Minute}
    deriving stock (Show, Eq, Generic)


defLastWrittenTime :: LastWrittenTime
defLastWrittenTime = MkLastWrittenTime Nothing Nothing 0


data DateTime = MkDateTime
    { season :: Season
    , day :: Day
    , year :: Year
    , time :: Time
    }
    deriving stock (Show, Eq, Generic)


defDateTime :: DateTime
defDateTime = MkDateTime Spring 0 0 defTime


data CharacterSkills = MkCharacterSkills
    { computers :: Int
    , electronics :: Int
    , fabrication :: Int
    , marksmanship :: Int
    , mechanics :: Int
    , survival :: Int
    , devices :: Int
    , healthCare :: Int
    , athletics :: Int
    , archery :: Int
    , bashingWeapons :: Int
    , cuttingWeapons :: Int
    , rifles :: Int
    , piercingWeapons :: Int
    , vehicles :: Int
    , social :: Int
    , foodHandling :: Int
    , tailoring :: Int
    , appliedScience :: Int
    }
    deriving stock (Show, Eq, Generic)


defCharacterSkills :: CharacterSkills
defCharacterSkills = MkCharacterSkills 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1


data CharacterStats = MkCharacterStats
    {strength :: Int, dexterity :: Int, intelligence :: Int, perception :: Int}
    deriving stock (Show, Eq, Generic)


data StatUpdate = MkStatUpdate {from :: Int, to :: Int}
    deriving stock (Show, Eq, Generic)


type StatKeyValues = (Map Text StatUpdate)


data StatKind init update = Initiation init | Update update
    deriving stock (Show, Eq, Generic)


-- TODO: Are we sure we want to use Text as symbol over just plain Char?
-- My assumption is that symbol may contain more than two characters, but I've
-- found only one so far. I use two here just to be safe but it's better to replace
-- with Char if we 100% sure symbol will be only 1 ASCII letter.
data CharacterKills = MkCharacterKills {count :: Int, symbol :: Text, name :: Text}
    deriving stock (Show, Eq, Generic)


data CharacterNPCKilled = MkCharacterNPCKilled {count :: Int, symbol :: Text, name :: Text}
    deriving stock (Show, Eq, Generic)


data Body = MkBody
    { stats :: Maybe (StatKind CharacterStats StatKeyValues)
    , skills :: Maybe (StatKind CharacterSkills StatKeyValues)
    , proficiencies :: Maybe (StatKind [Text] [Text])
    , mutations :: Maybe (StatKind [Text] [Text])
    , activeMissions :: Maybe (StatKind [Text] [Text])
    , newMissions :: Maybe (StatKind [Text] [Text])
    , gainedMutation :: Maybe (StatKind [Text] [Text])
    , lostMutation :: Maybe (StatKind [Text] [Text])
    , kills :: Maybe (StatKind [CharacterKills] [CharacterKills])
    , npcKilled :: Maybe (StatKind [CharacterNPCKilled] [CharacterNPCKilled])
    , note :: Text
    }
    deriving stock (Show, Eq, Generic)


defBody :: Body
defBody =
    MkBody
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        ""


type BodyParserT = StateT Body Parser


type Index = Int
type TotalIndexes = Int


type EntryParserT = StateT Entry Parser
data Entry = MkEntry
    { index :: Index
    , dateTime :: DateTime
    , lastWrittenTime :: Maybe LastWrittenTime
    , body :: Body
    }
    deriving stock (Show, Eq, Generic)


defEntry :: Entry
defEntry = MkEntry 1 defDateTime Nothing defBody


data Diary = MkDiary {totalIndexes :: Int, entries :: [Entry]}
    deriving stock (Show, Eq, Generic)


defDiary :: Diary
defDiary = MkDiary 0 []