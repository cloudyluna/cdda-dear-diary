{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use <$>" #-}

{- |
Module: CddaDearDiary.Internal
Copyright: (c) 2024 cloudyluna
SPDX-License-Identifier: BSD-3-Clause
Maintainer: cloudyluna

Internal module which contains diary sub-parsers. This is exported
in case user wish for more control.


/Users are __discouraged__ to import this module unless you know what you're doing./
Instead, use "CddaDearDiary" as much as possible.
-}
module CddaDearDiary.Internal (
    diaryParser,
    entriesParser,
    wholeEntryParser,
    entryPropertiesParser,
    entryIndexParser,
    timeParser,
    dateTimeParser,
    lastWrittenTimeParser,
) where

import CddaDearDiary.Internal.Parser.Body
import CddaDearDiary.Internal.Parser.Single
import CddaDearDiary.Types
import Control.Monad (unless, void)
import Control.Monad.State.Strict
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L


diaryParser :: [Text] -> Parser Diary
diaryParser extendedNewMissions = do
    (tix, entries) <- entriesParser extendedNewMissions
    pure $ MkDiary tix entries


entriesParser :: [Text] -> Parser (TotalIndexes, [Entry])
entriesParser extendedNewMissions = do
    -- TODO: Maybe we just use [Entry] length as the totalIndexes.
    xs <- unzip <$> many parser <* eof
    -- TODO: Avoid head if you can.
    pure (head $ fst xs, snd xs)
    where
        parser = runStateT (wholeEntryParser extendedNewMissions) defEntry


wholeEntryParser :: [Text] -> EntryParserT TotalIndexes
wholeEntryParser extendedNewMissions = do
    (index, totalIndexes, dateTime) <- lift entryPropertiesParser
    modify' $ \e -> e{index, dateTime}
    let isFirstEntry = index == 1

    -- Required for index > 1 entries.
    unless isFirstEntry $ do
        lastWrittenTime <- lift $ Just <$> lastWrittenTimeParser
        modify' $ \e -> e{lastWrittenTime}

    void $ spaceChar *> newline *> optional newline
    body <- lift $ parseBody isFirstEntry
    modify' $ \e -> e{body}

    pure totalIndexes
    where
        parseBody isFirstEntry =
            try (entryBodyParser isFirstEntry extendedNewMissions)
                -- This alternative is for the very last entry.
                <|> try
                    ( MkBody
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
                        <$> anythingParser
                    )


entryPropertiesParser :: Parser (Index, TotalIndexes, DateTime)
entryPropertiesParser = do
    (index, totalIndexes) <-
        entryKeywordParser *> space *> entryIndexParser <* comma <* space
    year <- yearParser <* space <* comma <* space
    season <- seasonParser <* comma <* space
    dateTime <- dateTimeParser season year
    pure (index, totalIndexes, dateTime)


entryIndexParser :: Parser (Int, Int)
entryIndexParser = do
    index <- L.decimal <* char '/'
    totalIndexes <- L.decimal
    pure (index, totalIndexes)


timeParser :: Parser Time
timeParser = do
    hour <- L.decimal <* colon
    minute <- L.decimal <* colon
    second <- L.decimal

    pure $ MkTime hour minute second


dateTimeParser :: Season -> Year -> Parser DateTime
dateTimeParser season year = do
    day <- string "day" *> space *> L.decimal <* space
    time <- timeParser <* comma

    pure $ MkDateTime{..}


lastWrittenTimeParser :: Parser LastWrittenTime
lastWrittenTimeParser = do
    day <- optional $ try (time "day") <* comma
    hour <- optional $ try (time "hour") <* comma
    minute <- time "minute" <* string " since last entry"
    pure $ MkLastWrittenTime day hour minute
    where
        time n =
            space
                *> L.decimal
                <* spaceChar
                <* string n
                <* optional (char 's')
