{-# LANGUAGE FlexibleContexts #-}

module Text.Reggie
  (
    module Text.Reggie.Parser
  , rGen
  )
where

import qualified Text.Reggie.Parser as P
import Text.Reggie.Prelude
import Text.Reggie.DSL
import Text.Reggie.RegexGenerator
import Text.Reggie.Parser (parse)
import Test.QuickCheck
import Debug.Trace

rGen :: String -> Gen String
rGen s = case parse s of
  Left e  -> error $ show e
  Right r -> genRegex r
