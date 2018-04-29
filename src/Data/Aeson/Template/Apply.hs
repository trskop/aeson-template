{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module:      Data.Aeson.Template.Apply
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Data.Aeson.Template.Apply
    ( Context
    , apply
    )
  where

import Control.Applicative (Applicative, pure)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

import qualified Data.Aeson.Types as Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy.Text (toStrict)
import qualified Data.Text.Template as Text.Template (renderA)

import Data.Aeson.Template.ValueTemplate (ValueTemplate(..))


type Context f = forall a. (Aeson.Value -> Maybe a) -> Text -> f a

apply
    :: forall f
    . Applicative f
    => Context f
    -> ValueTemplate
    -> f Aeson.Value
apply lookup = \case
    Value val ->
        pure val

    Variable var ->
        lookup Just var

    ObjectTemplate obj ->
        Aeson.Object <$> traverse (apply lookup) obj

    ArrayTemplate arr ->
        Aeson.Array <$> traverse (apply lookup) arr

    StringTemplate str ->
        Aeson.String . Lazy.Text.toStrict <$> renderStringTemplate str
      where
        renderStringTemplate t = Text.Template.renderA t lookupString
          where
            lookupString :: Text -> f Text
            lookupString = lookup $ \case
                Aeson.String s -> Just s
                _              -> Nothing
