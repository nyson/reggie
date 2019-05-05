{-# LANGUAGE LambdaCase, TupleSections #-}
module Text.Reggie.RegexGenerator where

import Text.Reggie.DSL
import Text.Reggie.Prelude
import Test.QuickCheck
import qualified Data.List.NonEmpty as NE

type ReggexInput = String

genRegex :: Reggex -> Gen ReggexInput
genRegex = genTerm . \case
  Enclosed _dir re -> re
  Reggex re -> re

genTerm :: RegexTerm -> Gen ReggexInput
genTerm = \case
  r@(String _) -> return $ pp r
  Star r -> concat <$> listOf (genTerm r)
  Plus r -> concat <$> listOf1 (genTerm r)
  Greedy _r -> error "Not handling greedies right now"
  RCharSet cs -> pp <$> genCharset cs
  Length r sMin sMax -> do
    len <- choose (sMin, sMax)
    concat <$> vectorOf len (genTerm r)
  AtLeast r sMin -> do
    NonNegative len <- arbitrary
    concat <$> vectorOf len (genTerm r)

genCharset :: CharSet -> Gen ReggexChar
genCharset cs = do
  let cs' = NE.toList $ unCharSet cs
      spans = map (1,) [ fromChar <$> choose (start, end)
                       | Span (AlphaNum start) (AlphaNum end) <- cs'
                       ]
      gen :: [(Int, Gen ReggexChar)]
      gen = case [ l | RCSLit l <- cs'] of
        [] | null spans -> error $ "no generator could be constructed for '" ++ pp cs ++ "'"
           | otherwise -> spans
        xs -> (1, elements xs): spans

  frequency gen
