{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Data.Aeson.Template.Parser
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Data.Aeson.Template.Parser
--  (
--  )
  where

import qualified Data.Attoparsec.ByteString as Attoparsec

import Data.Aeson.Template.ValueTemplate (ValueTemplate)


-- _Example_
--
-- Source:
--
--   { "foo": ${foo}
--   # Comment
--   , "bar": "alpha ${beta} gama" # Another comment
--   , "baz": [${elem0}, ${elem1}, ${elem2}]
--   , "qux": "literal string"
--   }
--
-- Result (foo = {"a": null}, beta = "beta", elem0 = true, elem1 = false,
-- elem2 = null):
--
--   {"foo":{"a":null},"bar":"alpha beta gama","baz":[true,false,null],"qux":"literal string"}

parseValueTemplate :: Attoparsec.Parser ValueTemplate
parseValueTemplate = parseValueTemplate -- TODO
