{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Data.Aeson.Template.KeyValue
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Data.Aeson.Template.KeyValue
    ( KeyValue(..)
    )
  where

import qualified Data.Aeson.Encoding.Internal as Aeson.Internal
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (singleton)
import Data.Text (Text)

import Data.Aeson.Template.ValueTemplate (ValueTemplate(..))
import Data.Aeson.Template.Encoding (pair, encodeValueTemplate)


class KeyValue kv where
    (.=) :: Text -> ValueTemplate -> kv

instance KeyValue (Text, ValueTemplate) where
    (.=) = (,)

instance KeyValue Aeson.Internal.Series where
    k .= v = pair k (encodeValueTemplate v)

instance KeyValue (HashMap Text ValueTemplate) where
    (.=) = HashMap.singleton
