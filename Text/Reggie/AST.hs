{-# LANGUAGE GADTs, LambdaCase #-}
module Text.Reggie.AST where

import Text.Reggie.Prelude
import Data.List (intercalate)

-- A disjunctive stream of regexes
newtype Regex = Regex { unRegex :: [RegexStream] }

instance PrettyPrint Regex where
  pp = intercalate "|" . map pp . unRegex

-- A conjunctive stream of regex terms
newtype RegexStream = RegexStream { unRegexStream :: [RegTerm] }

instance PrettyPrint RegexStream where
  pp = concatMap pp . unRegexStream

-- An item in a regex stream
data RegTerm
  = TChar RChar
  | TRep RegTerm Int (Maybe Int)
  | TCharset Bool [CharsetItem]

instance PrettyPrint RegTerm where
  pp = \case
    TChar c -> pp c
    TRep t 0 Nothing -> pp t ++ "*"
    TRep t 1 Nothing -> pp t ++ "+"
    TRep t n Nothing  -> concat
      [ pp t , "{", show n , ",}"]
    TRep t n (Just m)  -> concat
      [ pp t
      , "{", show n, ","
      , show m, "}"
      ]
    TCharset negated csis -> concat
      [ "["
      , if negated then "^" else mempty
      , concatMap pp csis
      , "]"
      ]

data RChar where
  Ch      :: Char   -> RChar
  Escaped :: Char   -> RChar
  Latin   :: [Char] -> RChar
  Unicode :: [Char] -> RChar
  Control :: Char   -> RChar

instance PrettyPrint RChar where
  pp = \case
    Ch      c  -> c:""
    Escaped c  -> '\\':c:""
    Latin   hx -> "\\x" ++ hx
    Unicode hx -> "\\u" ++ hx
    Control hx -> '\\':'c':hx:""

data CharsetItem
  = SSpan CsChar CsChar
  | SChar CsChar

instance PrettyPrint CharsetItem where
  pp = \case
    SSpan start end -> concat [pp start, "-", pp end]
    SChar c -> pp c

-- | Special chars 
newtype CsChar = CsChar {unCsChar :: Char}

instance PrettyPrint CsChar where
  pp = (:"") . unCsChar

type SpecialChars = ()

