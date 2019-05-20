{-# LANGUAGE FlexibleContexts #-}

module Text.Reggie
  ( parse, sampleReg, generator )
  where

import Text.Reggie.Parser (parse)
import Text.Reggie.RegexGenerator (mkGenRegex)

import Test.QuickCheck (generate, Gen)

generator :: String -> Gen String
generator str = either error id
  $ parse str >>= mkGenRegex

sampleReg :: String -> IO String
sampleReg = generate . generator
