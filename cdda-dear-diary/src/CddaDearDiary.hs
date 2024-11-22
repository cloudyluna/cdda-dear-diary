{- |
Module: CddaDearDiary
Copyright: (c) 2024 cloudyluna
SPDX-License-Identifier: BSD-3-Clause
Maintainer: cloudyluna

A very thin and simple parser for CDDA's diary format.

__Note__: Currently is developed for my personal purpose only, so it is
rather incomplete if you want more type safety out of the box.

This parser currently produces mostly types that are just aliases to primitive Haskell types.
-}
module CddaDearDiary (parseDiary, showParseError, module CddaDearDiary.Types) where

import CddaDearDiary.Internal (diaryParser)
import CddaDearDiary.Types
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Void (Void)
import Text.Megaparsec (
  ParseErrorBundle,
  ShowErrorComponent,
  errorBundlePretty,
  runParser,
 )
import Text.Megaparsec.Stream (TraversableStream, VisualStream)


{- | Parse a whole text of CDDA diary.

Caveat: currently `newMissions` key depends on 'CddaDearDiary.Internal.Parser.Body.allowedMissions'.

If your diary contains mission names that are not listed in that list, then this
function will most likely ended up generating garbled data format. Though, you can extend
this with 'extendedNewMissions' list.

Arguments:

  * Missions -/Additional new missions you would like to support./
  * Filename - /The file is only used in error messages and may be an empty string./
  * Source - /The actual diary source text./


Usage example:

@
{\-# LANGUAGE OverloadedStrings #-\}

module Main (main) where

import qualified Data.Text as T
import CddaDearDiary

main :: IO ()
main = do
  print (parseDiary ["Pick Plums"] "Appleton.txt" sourceCode)
    where
      sourceCode = "...."
@
-}
parseDiary ::
  [Text] ->
  FilePath ->
  Text ->
  Either
    ( ParseErrorBundle
        Text
        Void
    )
    Diary
parseDiary extendedNewMissions = runParser (diaryParser extendedNewMissions)


{- | Pretty-print error returned by 'parseDiary'.
This uses the default from "Megaparsec".
-}
showParseError ::
  forall s e.
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  ParseErrorBundle s e ->
  TL.Text
showParseError = TL.pack . errorBundlePretty