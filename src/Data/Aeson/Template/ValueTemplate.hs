{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module:      Data.Aeson.Template.ValueTemplate
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Data.Aeson.Template.ValueTemplate
    ( ValueTemplate(..)
    , ObjectTemplate
    , ArrayTemplate
    , StringTemplate
    , autoValue
    )
  where

import Data.Eq (Eq)
import Data.Function ((.))
import GHC.Generics (Generic)
import Text.Show (Show)

import qualified Data.Aeson as Aeson (ToJSON(toJSON))
import qualified Data.Aeson.Types as Aeson (Value(..))
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text.Template as Text (Template)
import Data.Vector (Vector)


data ValueTemplate
    = Value !Aeson.Value
    | Variable !Text
    | ObjectTemplate !ObjectTemplate
    | ArrayTemplate !ArrayTemplate
    | StringTemplate !StringTemplate
  deriving (Eq, Show, Generic)

type ObjectTemplate = HashMap Text ValueTemplate
type ArrayTemplate = Vector ValueTemplate
type StringTemplate = Text.Template

autoValue :: Aeson.ToJSON a => a -> ValueTemplate
autoValue = Value . Aeson.toJSON
{-# INLINE autoValue #-}
