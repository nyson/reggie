{-# LANGUAGE LambdaCase #-}
module Text.Reggie.RegexGenerator where

import Text.Reggie.AST
import Text.Reggie.Prelude
import Test.QuickCheck
import Control.Monad (mapM)
import Control.Monad.Except

type ReggexInput = String
type RGen = ExceptT String Gen ()

mkGenRegex :: Regex -> Either String (Gen String)
mkGenRegex = (Right . oneof =<<) . mapM mkGenStream . flatten

mkGenStream :: RegexStream -> Either String (Gen String)
mkGenStream (RegexStream rs) = fmap concat . sequence <$> mapM mkGenTerm rs

mkGenTerm :: RegTerm -> Either String (Gen String)
mkGenTerm = \case
  TChar c    -> return . return $ c:""
  TScope r   -> mkGenRegex r
  TEscaped e -> mkGenEscaped e
  TCharset negated cs -> mkGenCharset negated cs
  TRep t start end -> mkGenRepetitions t start end

mkGenEscaped :: REscaped -> Either String (Gen String)
mkGenEscaped = return . return . pp

mkGenRepetitions :: RegTerm -> Int -> Maybe Int -> Either String (Gen String)
mkGenRepetitions term low = \case
  Nothing -> do
    gen <- mkGenTerm term
    let conc2 a b = concat $ a <> b
    return $ conc2 <$> vectorOf low gen <*> listOf gen
  Just top -> do
    gen <- mkGenTerm term
    return $ choose (low, top) >>= fmap concat . sequence . flip replicate gen

mkGenCharset :: Bool -> [CharsetItem] -> Either String (Gen String)
mkGenCharset True  = const $ Left "negated charset not supported yet"
mkGenCharset False = \case
  [] -> Left "An empty charset will never match a string"
  xs@(_:_) -> oneof <$> mapM mkGenCharsetItem xs

mkGenCharsetItem :: CharsetItem -> Either String (Gen String)
mkGenCharsetItem = \case
  SSpan a b -> return $ (:"") <$> choose (a, b)
  SChar c -> return $ return (c:"")
