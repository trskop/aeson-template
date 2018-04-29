{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Module:      Data.Aeson.Template.Encoding
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Data.Aeson.Template.Encoding
    ( TemplateEncoding
    , encodeValueTemplate
    , pair
    )
  where

import Data.List (foldr)
import Data.Monoid ((<>), mempty)
import Data.Function ((.))

import Data.Aeson.Encoding.Internal ((><))
import qualified Data.Aeson.Encoding.Internal as Aeson.Internal
import qualified Data.Aeson.Types as Aeson (Value(..))
import qualified Data.ByteString.Builder as Builder (byteString)
import qualified Data.HashMap.Strict as HashMap (foldrWithKey)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.Template as Text.Template (showTemplate)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (toList)

import Data.Aeson.Template.ValueTemplate
    ( ArrayTemplate
    , ObjectTemplate
    , StringTemplate
    , ValueTemplate(..)
    )


type TemplateEncoding = Aeson.Internal.Encoding' ValueTemplate

encodeValueTemplate :: ValueTemplate -> TemplateEncoding
encodeValueTemplate = \case
    Value val -> value val
    Variable var -> variable var
    ObjectTemplate obj -> objectTemplate obj
    ArrayTemplate arr -> arrayTemplate arr
    StringTemplate str -> stringTemplate str

value :: Aeson.Value -> TemplateEncoding
value = Aeson.Internal.retagEncoding . Aeson.Internal.value

variable :: Text -> TemplateEncoding
variable v = Aeson.Internal.Encoding ("${" <> v' <> "}")
  where
    v' = Builder.byteString (Text.encodeUtf8 v)

objectTemplate :: ObjectTemplate -> TemplateEncoding
objectTemplate =
    dict Aeson.Internal.text encodeValueTemplate HashMap.foldrWithKey

arrayTemplate :: ArrayTemplate -> TemplateEncoding
arrayTemplate = vector encodeValueTemplate

stringTemplate :: StringTemplate -> TemplateEncoding
stringTemplate = Aeson.Internal.text . Text.Template.showTemplate

pair :: Text -> TemplateEncoding -> Aeson.Internal.Series
pair k = Aeson.Internal.Value . pair' (Aeson.Internal.text k)

pair'
    :: Aeson.Internal.Encoding' a
    -> Aeson.Internal.Encoding' b
    -> Aeson.Internal.Encoding' c
pair' k v =
    Aeson.Internal.retagEncoding k
        >< Aeson.Internal.colon
        >< Aeson.Internal.retagEncoding v

pairs :: Aeson.Internal.Series -> TemplateEncoding
pairs = \case
    Aeson.Internal.Value v ->
        Aeson.Internal.openCurly
            >< Aeson.Internal.retagEncoding v
            >< Aeson.Internal.closeCurly

    Aeson.Internal.Empty ->
        Aeson.Internal.retagEncoding Aeson.Internal.emptyObject_

-- | Encode as JSON object
dict
    :: (k -> Aeson.Internal.Encoding' Text)
    -- ^ key encoding
    -> (v -> TemplateEncoding)
    -- ^ value encoding
    -> (forall a. (k -> v -> a -> a) -> a -> m -> a)
    -- ^ @foldrWithKey@ - indexed fold
    -> m
    -- ^ container
    -> TemplateEncoding
dict encodeKey encodeVal foldrWithKey = pairs . foldrWithKey go mempty
  where
    go k v c = Aeson.Internal.Value (pair' (encodeKey k) (encodeVal v)) <> c

list :: (a -> TemplateEncoding) -> [a] -> TemplateEncoding
list to' = \case
    [] ->
        Aeson.Internal.retagEncoding Aeson.Internal.emptyArray_

    x : xs ->
        Aeson.Internal.openBracket >< commas x xs >< Aeson.Internal.closeBracket
  where
    commas x xs = to' x >< foldr go Aeson.Internal.empty xs
      where
        go v vs = Aeson.Internal.comma >< to' v >< vs

vector :: (a -> TemplateEncoding) -> Vector a -> TemplateEncoding
vector to' = list to' . Vector.toList
