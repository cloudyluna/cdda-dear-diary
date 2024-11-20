{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module: CddaDearDiary.Aeson
Copyright: (c) 2024 cloudyluna
SPDX-License-Identifier: BSD-3-Clause
Maintainer: cloudyluna

Aeson's orphan instances for CddaDearDiary. /Use with caution/.
-}
module CddaDearDiary.Aeson () where

import CddaDearDiary.Types
import Data.Aeson
import GHC.Generics (Generic, Rep)


omitNulls :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Value
omitNulls = genericToJSON defaultOptions{omitNothingFields = True}


instance ToJSON Season where
    toJSON = omitNulls


instance ToJSON Time where
    toJSON = omitNulls


instance ToJSON LastWrittenTime where
    toJSON = omitNulls


instance ToJSON DateTime where
    toJSON = omitNulls


instance ToJSON CharacterStats where
    toJSON = omitNulls


instance ToJSON CharacterSkills where
    toJSON = omitNulls


instance ToJSON CharacterKills where
    toJSON = omitNulls


instance ToJSON CharacterNPCKilled where
    toJSON = omitNulls


instance ToJSON StatUpdate where
    toJSON = omitNulls


instance (ToJSON a, ToJSON b) => ToJSON (StatKind a b) where
    toJSON = omitNulls


instance ToJSON Body where
    toJSON = omitNulls


instance ToJSON Entry where
    toJSON = omitNulls


instance ToJSON Diary where
    toJSON = omitNulls
