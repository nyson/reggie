{-# LANGUAGE GADTs, LambdaCase #-}

module Text.Reggie.DSL where

import Data.Bifunctor
import Data.Char (isAlphaNum)
import System.Random
import Text.Reggie.Prelude
import Test.QuickCheck
import Data.List.NonEmpty as NE

data Reggex where
  -- p$, ^p and ^p$
  Reggex :: RegexTerm -> Reggex
  Enclosed :: Border -> RegexTerm -> Reggex
  deriving ( Eq, Show, Read )

border :: Bool -> Bool -> Maybe Border
border False False = Nothing
border True  True  = Just Both
border True  False = Just Start
border False True  = Just End

instance Arbitrary Reggex where
  arbitrary = frequency $
    [ (10, Reggex <$> arbitrary)
    , (1, Enclosed <$> arbitrary <*> arbitrary)
    ]
instance PrettyPrint Reggex where
  pp (Reggex r) = pp r
  pp (Enclosed b r) = concat
    [ if b /= End then "^" else mempty
    , pp r
    , if b /= Start then "$" else mempty
    ]

data RegexTerm where
  String :: NE.NonEmpty ReggexChar -> RegexTerm
  -- p*
  Star :: RegexTerm -> RegexTerm
  -- p+
  Plus :: RegexTerm -> RegexTerm
  -- p?
  Greedy :: RegexTerm -> RegexTerm
  -- "ab" and c to f = [abc-f]
  RCharSet :: CharSet -> RegexTerm
  -- Length p 10 20 = p{10, 20}
  Length :: RegexTerm -> Int -> Int -> RegexTerm
  -- AtLeast p 10 = p{10, }
  AtLeast :: RegexTerm -> Int -> RegexTerm
  deriving (Eq, Show, Read)

instance Arbitrary RegexTerm where
  arbitrary = frequency
    [ (100, String    <$> neList arbitrary)
    , (50,  Star      <$> arbitrary)
    , (50,  Plus      <$> arbitrary)
    , (50,  Greedy    <$> arbitrary)
    , (10,  Star      <$> arbitrary)
    , (10,  RCharSet  <$> arbitrary)
    , (10,  Length    <$> arbitrary <*> genPositiveInteger <*> genPositiveInteger)
    , (10,  AtLeast   <$> arbitrary <*> genPositiveInteger)
    ]

data Border = Start | End | Both
  deriving (Eq, Show, Read)

instance Arbitrary Border where
  arbitrary = elements [Start, End, Both]

singleChar :: RegexTerm -> Bool
singleChar (String (_ :| [])) = True
singleChar _                  = False

instance PrettyPrint RegexTerm where
  pp = let encapsulate rx = if singleChar rx
                            then pp rx
                            else "(" ++ pp rx ++ ")"
       in \case
    String rxc -> concatMap pp rxc
    Star rx -> encapsulate rx ++ "*"
    Plus rx -> encapsulate rx ++ "+"
    Greedy rx -> encapsulate rx ++ "?"
    RCharSet a -> pp a
    Length rx mi mx -> encapsulate rx
                       ++ concat ["{", show mi, ", ", show mx, "}"]
    AtLeast rx mi -> encapsulate rx ++ concat ["{", show mi, " , }"]

data CharSet
  = CharSet {unCharSet :: NonEmpty CharSetItem}
  | NCharSet {unCharSet :: NonEmpty CharSetItem}
  deriving (Eq, Show, Ord, Read)

negatedCharSet :: CharSet -> Bool
negatedCharSet (NCharSet _) = True
negatedCharSet _            = False

instance PrettyPrint CharSet where
  pp cs = concat
    [ "["
    , negatedCharSet cs ?? "^"
    , concatMap pp . NE.toList . unCharSet $ cs
    , "]"
    ]

data CharSetItem = RCSLit ReggexChar
                 | Span ReggexChar ReggexChar
                 deriving (Eq, Show, Ord, Read)

instance Arbitrary CharSet where
  arbitrary = oneof
    [ CharSet  <$> neList arbitrary
    , NCharSet <$> neList arbitrary
    ]

instance Arbitrary CharSetItem where
  arbitrary = frequency
    [ (10, RCSLit <$> arbitrary)
    , (1, Span   <$> arbitrary <*> arbitrary)
    ]

instance PrettyPrint CharSetItem where
  pp (RCSLit c) = pp c
  pp (Span start end)  = pp start ++ "-" ++ pp end

data ReggexChar
  = AlphaNum Char
  | Dot            -- .
  | Null           -- \0
  | Tab            -- \t
  | Newline        -- \n
  | VTab           -- \v
  | FormFeed       -- \f
  | CarReturn      -- \r
  | Latin String   -- \xNN
  | Unicode String -- \u(Hexadecimal)
  | Control Char   -- \c(Char)
  | Whitespace     -- \s
  | Digit          -- \d
  | Word           -- \w
  | Backspace      -- \\ or \b
  deriving (Show, Eq, Ord, Read)

instance Arbitrary ReggexChar where
  arbitrary = frequency
    [ (100, AlphaNum <$> elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
    , (2,   elements [ Dot, Null, Tab, Newline, VTab, FormFeed
                     , CarReturn, Whitespace, Digit, Word
                     , Backspace]
      )

    , (1, Latin   <$> genHex 2)
    , (1, Unicode <$> genHex 4)
    , (1, Control <$> elements ['a'..'z'])
    ]
instance PrettyPrint ReggexChar where
  pp = \case
    Dot        -> "."
    AlphaNum c -> [c]
    Null       -> "\\0"
    Tab        -> "\\t"
    Newline    -> "\\n"
    VTab       -> "\\v"
    FormFeed   -> "\\f"
    CarReturn  -> "\\r"
    Latin hx   -> "\\x" ++ hx
    Unicode hx -> "\\u" ++ hx
    Control c  -> "\\c" ++ [c]
    Whitespace -> "\\s"
    Digit      -> "\\d"
    Word       -> "\\w"
    Backspace  -> "\\b"

fromChar :: Char -> ReggexChar
fromChar = \case
  '.'  -> Dot
  '\0' -> Null
  '\t' -> Tab
  '\n' -> Newline
  '\f' -> FormFeed
  '\r' -> CarReturn
  ' '  -> Whitespace
  c | isAlphaNum c -> AlphaNum c
    | otherwise    -> error $ c:" not supported!"

toChar :: ReggexChar -> Char
toChar = \case
  AlphaNum c -> c
  Dot -> '.'
  Null -> '\0'
  Tab -> '\t'
  Newline -> '\n'
  FormFeed -> '\f'
  CarReturn -> '\r'
  Whitespace -> ' '
  c -> error $ show c ++ " not supported!"
instance Random ReggexChar where
  randomR (rcs, rce) g
    = first fromChar $ randomR (toChar rcs, toChar rce) g
  random g = first fromChar $ random g
