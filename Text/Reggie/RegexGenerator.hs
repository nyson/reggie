{-# LANGUAGE LambdaCase, TupleSections #-}
module Text.Reggie.RegexGenerator where

import Text.Reggie.AST
import Text.Reggie.Prelude
import Test.QuickCheck
import Control.Monad (mapM)
import qualified Data.List.NonEmpty as NE

type ReggexInput = String

mkGenRegex :: Regex -> Gen String
mkGenRegex (Regex r) = oneof (map mkGenStream r)

mkGenStream :: RegexStream -> Gen String
mkGenStream (RegexStream rs) = concat <$> mapM mkGenTerm rs

mkGenTerm :: RegTerm -> Gen String
mkGenTerm = \case 
  TChar c -> return $ c:""
  TScope r -> mkGenRegex r
  TEscaped e -> mkGenEscaped e
  TCharset negated cs -> mkGenCharset negated cs
  TRep t start end -> mkGenRepetitions t start end

mkGenEscaped :: REscaped -> Gen String
mkGenEscaped = return . pp 

mkGenRepetitions :: RegTerm -> Int -> Maybe Int -> Gen String
mkGenRepetitions term low = \case
  Nothing -> do 
    let conc2 a b = concat $ a <> b
    conc2 <$> vectorOf low gen <*> listOf gen 
  Just top -> choose (low, top) 
              >>= fmap concat . sequence . flip replicate gen
  
  where gen = mkGenTerm term
    

mkGenCharset :: Bool -> [CharsetItem] -> Gen String
mkGenCharset True  = error "negated charset not supported yet"
mkGenCharset False = oneof . map mkGenCharsetItem

mkGenCharsetItem :: CharsetItem -> Gen String
mkGenCharsetItem = \case
  SSpan a b -> (:"") <$> choose (a, b)
  SChar c -> return (c:"")
-- mkmkGenRegex :: Reggex -> mkmkGen ReggexInput
-- mkmkGenRegex = mkmkGenTerm . \case
--   Enclosed _dir re -> re
--   Reggex re -> re

-- mkmkGenTerm :: RegexTerm -> mkGen ReggexInput
-- mkGenTerm = \case
--   r@(String _) -> return $ pp r
--   Star r -> concat <$> listOf (mkGenTerm r)
--   Plus r -> concat <$> listOf1 (mkGenTerm r)
--   Greedy _r -> error "Not handling greedies right now"
--   RCharSet cs -> pp <$> mkGenCharset cs
--   Length r sMin sMax -> do
--     len <- choose (sMin, sMax)
--     concat <$> vectorOf len (mkGenTerm r)
--   AtLeast r sMin -> do
--     NonNegative len <- arbitrary
--     concat <$> vectorOf len (mkGenTerm r)

-- mkGenCharset :: CharSet -> mkGen ReggexChar
-- mkGenCharset cs = do
--   let cs' = NE.toList $ unCharSet cs
--       spans = map (1,) [ fromChar <$> choose (start, end)
--                        | Span (AlphaNum start) (AlphaNum end) <- cs'
--                        ]
--       mkGen :: [(Int, mkGen ReggexChar)]
--       mkGen = case [ l | RCSLit l <- cs'] of
--         [] | null spans -> error $ "no mkGenerator could be constructed for '" ++ pp cs ++ "'"
--            | otherwise -> spans
--         xs -> (1, elements xs): spans

--   frequency mkGen
