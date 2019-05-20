{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Reggie.Prelude
  ( genPositiveInteger, genHex, genValidChar, genLetterChar
  , NonEmpty(..), PrettyPrint(..)
  , StringConv(..)
  , module Data.Bifunctor
  ) where

import Control.Applicative (liftA2)
import Test.QuickCheck
import Data.Bifunctor
import Data.List.NonEmpty
import Data.Char (chr)

class PrettyPrint a where
  pp :: a -> String

class StringConv a b where
  conv :: a -> b

instance StringConv String String where
  conv = id

genValidChar :: Gen Char
genValidChar = elements ['a'..'z']--chr <$> elements valids
  where valids = [32..39] ++ [46..90] -- ++ [94..121] ++ [92, 125, 126]

genLetterChar :: Gen Char
genLetterChar = elements $ ['a'..'z'] ++ ['A'..'Z']

-- | Generates a positive integer
genPositiveInteger :: Gen Int
genPositiveInteger = abs <$> arbitrary

-- | generates a sized of hex values
genHex :: Int -> Gen String
genHex size
  | size <= 0 = return ""
  | otherwise = liftA2 (:) (elements hexChars) (genHex $ size - 1)
  where hexChars = ['a' .. 'f'] ++ ['A' .. 'F'] ++ ['0' .. '9']
