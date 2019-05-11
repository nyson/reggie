{-# LANGUAGE FlexibleContexts #-}

module Text.Reggie 
  ( parse, sampleReg, generator )
  where

import Text.Reggie.Parser (parse)
import Text.Reggie.RegexGenerator (mkGenRegex)
import Test.QuickCheck (generate, Gen(..))

generator :: String -> Gen String
generator str = case parse str of
  Right rx -> mkGenRegex rx
  Left err -> error "wtf are you doing thats not a regex"

sampleReg :: String -> IO String  
sampleReg str = case parse str of
  Right rx -> generate $ mkGenRegex rx
  Left err -> error "wtf are you doing thats not a regex"