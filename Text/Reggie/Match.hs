
{-# LANGUAGE LambdaCase #-}
{- | Match.hs

Module responsible for matching strings with regexes

-}
module Text.Reggie.Match (match, match_) where

import Prelude


import Text.Reggie.AST
import Control.Monad.State
import Control.Monad.Except

import Data.List (uncons)
import Data.Char (toLower)
import Data.Either (isRight)

data MatchErr = MatchErr {unErr :: String}
              | Mismatch
  deriving (Show, Eq)

mismatch :: Matcher a
mismatch = throwError Mismatch

err :: String -> Matcher a
err = throwError . MatchErr

type Matcher = ExceptT MatchErr (State String)

mHead :: Matcher Char
mHead = get >>= \case
  (c:_) -> do
    modify tail
    return c
  []     -> err "Unexpected end of input"

-- mTakeExactly :: Int -> Matcher Char
-- mTakeExactly n = get >>= \case
--   xs | length xs >= n -> modify (drop n) $> take n xs
--   _ -> err "Unexpected end of input"

safeSplitAt :: Int -> [a] -> Maybe ([a], [a])
safeSplitAt i xs | length xs >= i = Just $ splitAt i xs
safeSplitAt _ _ = Nothing

dropMatch :: Char -> Matcher ()
dropMatch ch = get >>= \case
  (matchChar:_cs)
    | ch == matchChar -> modify tail
    | otherwise -> err $ concat
      [ "Mismatched char '", [matchChar]
      , "', expecting '", [ch], "'"
      ]
  [] -> err $ "Expected '" ++ [ch] ++ "', got end of input"

dropMatch' :: Char -> Matcher ()
dropMatch' ch = get >>= \case
  (matchChar:_cs)
    | toLower ch == toLower matchChar -> modify tail
    | otherwise -> err $ concat
      [ "Mismatched char '", [matchChar]
      , "', expecting '", [ch], "'"
      ]
  [] -> err $ "Expected '" ++ [ch] ++ "', got end of input"

match_ :: Regex -> String -> Bool
match_ reg = either (const False) (const True) . match reg

match :: Regex -> String -> Either MatchErr ()
match = evalState . runExceptT . matchRegex

matchRegex :: Regex -> Matcher ()
matchRegex rs = do
  str <- get
  let evaled = map (flip evalState str . runExceptT . matchStream) 
             $ flatten rs
  unless (any isRight evaled)
    mismatch

matchWith :: (String -> Maybe (a, String)) -> (a -> Bool) -> Matcher ()
matchWith consumer f = do
  result <- consumer <$> get
  case result of
    Just (munch, rest) -> do
      put rest
      when (not $ f munch) mismatch
    Nothing -> mismatch

matchStream :: RegexStream -> Matcher ()
matchStream (RegexStream []) = return ()
matchStream (RegexStream ss) = mapM_ matchTerm ss

matchTerm :: RegTerm -> Matcher ()
matchTerm = \case
  TScope r
    -> matchRegex r
  TChar rCh
    -> matchWith uncons (== rCh)
  TEscaped esc
    -> matchEscaped esc
  TRep term start end
    -> matchRepetitions term start end
  TCharset negated charset
    -> matchCharset negated charset

matchEscaped :: REscaped -> Matcher ()
matchEscaped esc = dropMatch '\\' >> case esc of
  Escaped ch  -> matchWith uncons (== ch)
  Latin hex   -> dropMatch' 'x' >> matchWith (safeSplitAt 2) (== hex)
  Unicode hex -> dropMatch' 'u' >> matchWith (safeSplitAt 4) (== hex)
  Control ch  -> dropMatch' 'c' >> matchWith uncons (== ch)

saveMismatch :: Matcher () -> Matcher () -> Matcher ()
saveMismatch matcher continuation = do
  st <- get
  (matcher >> continuation) `catchError` (\case
    Mismatch -> put st *> return ()
    other    -> throwError other)

matchRepetitions :: RegTerm -> Int -> Maybe Int -> Matcher ()
matchRepetitions t 0 upperBound = case upperBound of 
  Just n -> saveMismatch (matchTerm t) (matchRepetitions t 0 (Just $ n - 1))
  _      -> return ()
matchRepetitions t i mx = matchTerm t >> matchRepetitions t (i-1) mx


matchCharset :: Bool -> [CharsetItem] -> Matcher ()
matchCharset negated xs = do
  ch <- mHead
  let missF | negated   = not . any (matchCharsetItem ch)
            | otherwise = any (matchCharsetItem ch)
  unless (missF xs) mismatch

matchCharsetItem :: Char -> CharsetItem -> Bool
matchCharsetItem c = \case
  SSpan start end -> c >= start && c <= end
  SChar sc -> c == sc
