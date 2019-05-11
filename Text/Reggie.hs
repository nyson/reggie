{-# LANGUAGE FlexibleContexts #-}

module Text.Reggie
  (
    module Text.Reggie.ASTParser
  )
where

import Text.Reggie.Prelude
import Text.Reggie.RegexGenerator
import Text.Reggie.ASTParser (parse)
import Test.QuickCheck
import Debug.Trace

-- rGen :: String -> Gen String
-- rGen s = case parse s of
--   Left e  -> error $ show e
--   Right r -> genRegex r
