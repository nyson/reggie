{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Text.Reggie.Prelude
  ( safeHead, neList
  , genPositiveInteger, genHex, genValidChar, genLetterChar
  , NonEmpty(..), PrettyPrint(..)
  , StringConv(..)
  , (?), (??), (€), defCond
  , module Data.Bifunctor
  ) where

import Control.Applicative (liftA2)
import qualified Data.List.NonEmpty as NE
import Test.QuickCheck
import Data.Default
import Data.Bifunctor

import Data.List.NonEmpty

class PrettyPrint a where
  pp :: a -> String

class StringConv a b where
  conv :: a -> b

instance StringConv String String where
  conv = id

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

genValidChar :: Gen Char
genValidChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

genLetterChar :: Gen Char
genLetterChar = elements $ ['a'..'z'] ++ ['A'..'Z']

-- | generates a sized of hex values
genHex :: Int -> Gen String
genHex size
  | size <= 0 = return ""
  | otherwise = liftA2 (:) (elements hexChars) (genHex $ size - 1)
  where hexChars = ['a' .. 'f'] ++ ['A' .. 'F'] ++ ['0' .. '9']

genPositiveInteger :: Gen Int
genPositiveInteger = choose (0, 1000)

-- | generates a NonEmpty list of values
neList :: Gen a -> Gen (NE.NonEmpty a)
neList = fmap NE.fromList . listOf1

infixl 6 ?
(?) :: Bool -> a -> Maybe a
c ? a
  | c = Just a
  | otherwise = Nothing

infixl 6 ??
(??) :: Default a => Bool -> a -> a
(??) = defCond

defCond :: Default a => Bool -> a -> a
defCond conditional a
  | conditional = a
  | otherwise   = def

infixl 5 €
(€) :: Maybe a -> a -> a
ca € b = case ca of
  Just a  -> a
  Nothing -> b
