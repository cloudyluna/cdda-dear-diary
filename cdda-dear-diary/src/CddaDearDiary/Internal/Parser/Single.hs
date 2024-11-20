module CddaDearDiary.Internal.Parser.Single where

import CddaDearDiary.Types
import Data.Text (Text)
import Text.Megaparsec.Char

import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer qualified as L


{-# INLINE colon #-}
colon :: Parser Char
colon = char ':'


{-# INLINE comma #-}
comma :: Parser Char
comma = char ','


anythingParser :: Parser Text
anythingParser = T.pack <$> many anySingle <?> "Any char for note"


{-# INLINE entryKeywordParser #-}
entryKeywordParser :: Parser Text
entryKeywordParser = string "Entry:"


toSeason :: Text -> Season
toSeason = \case
    "Spring" -> Spring
    "Summer" -> Summer
    "Fall" -> Fall
    "Winter" -> Winter
    _other -> error "impossible: this code used in a context that it can't fail"


{-# INLINE seasonParser #-}
seasonParser :: Parser Season
seasonParser = toSeason <$> choice ["Spring", "Summer", "Fall", "Winter"]


{-# INLINE yearParser #-}
yearParser :: Parser Year
yearParser = string "Year" *> space *> L.decimal
