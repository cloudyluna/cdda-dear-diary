{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Expectations where

import CddaDearDiary.Types
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T


dateTime :: (Text, DateTime)
dateTime =
    ( "day 3 12:02:12,"
    , MkDateTime{season = Summer, year = 1, day = 3, time = MkTime 12 2 12}
    )


entryProperties :: (Text, (Int, Int, DateTime))
entryProperties =
    ( "Entry: 1/3, Year 1, Spring, day 61 12:10:30,"
    , (1, 3, MkDateTime{season = Spring, year = 1, day = 61, time = MkTime 12 10 30})
    )


wholeEntry :: (Text, Entry)
wholeEntry =
    ( "Entry: 1/3, Year 1, Spring, day 61 12:10:30,\n\n"
    , let dt = MkDateTime{season = Spring, year = 1, day = 61, time = MkTime 12 10 30}
       in MkEntry
            { index = 1
            , dateTime = dt
            , lastWrittenTime = Nothing
            , body = defBody
            }
    )


wholeEntry2 :: (Text, Entry)
wholeEntry2 =
    ( "Entry: 3/3, Year 1, Spring, day 61 12:10:30, 1 hour, 52 minutes since last entry\n\n"
    , let dt = MkDateTime{season = Spring, year = 1, day = 61, time = MkTime 12 10 30}
       in MkEntry
            { index = 3
            , dateTime = dt
            , lastWrittenTime = Just $ MkLastWrittenTime Nothing (Just 1) 52
            , body = defBody
            }
    )


entriesData :: Text
entriesData =
    T.unlines
        [ "Entry: 1/2, Year 1, Spring, day 61 10:18:02,"
        , ""
        , ""
        , "Stats:"
        , "Strength: 12"
        , "Dexterity: 14"
        , "Intelligence: 18"
        , "Perception: 22"
        , ""
        , "Skills:"
        , "computers: 1"
        , "electronics: 1"
        , "fabrication: 1"
        , "marksmanship: 1"
        , "mechanics: 1"
        , "survival: 1"
        , "devices: 1"
        , "health care: 1"
        , "athletics: 1"
        , "archery: 1"
        , "bashing weapons: 1"
        , "cutting weapons: 1"
        , "rifles: 1"
        , "piercing weapons: 1"
        , "vehicles: 1"
        , "social: 1"
        , "food handling: 1"
        , "tailoring: 1"
        , "applied science: 1"
        , ""
        , "Proficiencies:"
        , "Lockpicking"
        , "Wound Care"
        , ""
        , "Mutations:"
        , "Bookworm"
        , ""
        , "Active missions:"
        , "Find more cosplay suits"
        , " "
        , "BANANANA"
        , ""
        , "Entry: 2/3, Year 1, Spring, day 61 10:18:02, 8 hours, 15 minutes since last entry"
        , ""
        , "Skills: "
        , "computers: 1 -> 9"
        , " "
        , "Proficiencies: "
        , "Principles of Zombie Biology"
        , " "
        , "New missions:"
        , "Reach Refugee Center"
        , " "
        , "Find Antibiotics"
        , " "
        , "Gained Mutation: "
        , "Sharper"
        , "Strong"
        , " "
        , "Lost Mutation: "
        , "Rough"
        , "Weak"
        , " "
        , "Kills: "
        , "   2 @ feral mechanics"
        , "   11 Z decayed zombies"
        , "   62 Z zombies"
        , " "
        , "NPC Killed: "
        , "   1 @ Ai Sylvia"
        , " "
        , "MEWOEWOE*@*#@Jjjwajsaj!&@&!HHA"
        , "   "
        ]


entries :: (Text, (TotalIndexes, [Entry]))
entries =
    ( entriesData
    ,
        ( 2
        ,
            [ MkEntry
                { index = 1
                , dateTime =
                    MkDateTime{season = Spring, day = 61, year = 1, time = MkTime 10 18 2}
                , lastWrittenTime = Nothing
                , body =
                    MkBody
                        { stats =
                            Just $
                                Initiation
                                    ( MkCharacterStats
                                        { strength = 12
                                        , dexterity = 14
                                        , intelligence = 18
                                        , perception = 22
                                        }
                                    )
                        , skills = Just $ Initiation defCharacterSkills
                        , proficiencies = Just $ Initiation ["Lockpicking", "Wound Care"]
                        , mutations = Just $ Initiation ["Bookworm"]
                        , activeMissions = Just $ Initiation ["Find more cosplay suits"]
                        , newMissions = Nothing
                        , gainedMutation = Nothing
                        , lostMutation = Nothing
                        , kills = Nothing
                        , npcKilled = Nothing
                        , note = "BANANANA\n\n"
                        }
                }
            , MkEntry
                { index = 2
                , dateTime =
                    MkDateTime{season = Spring, day = 61, year = 1, time = MkTime 10 18 2}
                , lastWrittenTime = Just $ MkLastWrittenTime Nothing (Just 8) 15
                , body =
                    MkBody
                        { stats = Nothing
                        , skills = Just $ Update $ M.fromList [("computers", MkStatUpdate 1 9)]
                        , proficiencies = Just $ Update ["Principles of Zombie Biology"]
                        , mutations = Nothing
                        , activeMissions = Nothing
                        , newMissions = Just $ Update ["Reach Refugee Center", "Find Antibiotics"]
                        , gainedMutation = Just $ Update ["Sharper", "Strong"]
                        , lostMutation = Just $ Update ["Rough", "Weak"]
                        , kills =
                            Just $
                                Update
                                    [ MkCharacterKills{count = 2, symbol = "@", name = "feral mechanics"}
                                    , MkCharacterKills{count = 11, symbol = "Z", name = "decayed zombies"}
                                    , MkCharacterKills{count = 62, symbol = "Z", name = "zombies"}
                                    ]
                        , npcKilled = Just $ Update [MkCharacterNPCKilled 1 "@" "Ai Sylvia"]
                        , note = "MEWOEWOE*@*#@Jjjwajsaj!&@&!HHA\n   \n"
                        }
                }
            ]
        )
    )