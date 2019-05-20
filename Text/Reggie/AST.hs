{-# LANGUAGE GADTs, LambdaCase, TypeApplications #-}
module Text.Reggie.AST where

import Test.QuickCheck
import Text.Reggie.Prelude
import Data.List (find)
import Data.Char (chr, ord)

-- A disjoint stream of regexes
data Regex = Union Regex Regex
           | Single RegexStream
  deriving (Show, Read)

flatten :: Regex -> [RegexStream]
flatten = \case 
  Union l r -> flatten l ++ flatten r
  Single r  -> [r]

instance Eq Regex where
  r1 == r2 = flatten r1 == flatten r2

instance Arbitrary Regex where
  arbitrary = Single <$> arbitrary

instance PrettyPrint Regex where
  pp = \case 
    Union left right -> pp left <> "|" <> pp right
    Single rstr -> pp rstr

-- A conjunctive stream of regex terms
newtype RegexStream = RegexStream [RegTerm]
  deriving (Show, Eq, Read)

unRegexStream :: RegexStream -> [RegTerm]
unRegexStream (RegexStream terms) = terms

instance Arbitrary RegexStream where
  arbitrary = RegexStream <$> listOf arbitrary

instance PrettyPrint RegexStream where
  pp (RegexStream rstr) = concatMap pp rstr

-- An item in a regex stream
data RegTerm
  = TScope Regex
  | TChar Char
  | TEscaped REscaped
  | TRep RegTerm Int (Maybe Int)
  | TCharset Bool [CharsetItem]
  deriving (Show, Eq, Read)

instance Arbitrary RegTerm where
  arbitrary = do
    term <- frequency
      [ (1,   TScope   <$> scale (`div` 10) arbitrary)
      , (100, TChar    <$> genValidChar)
      , (5,   TEscaped <$> arbitrary)
      , (10,  TCharset <$> arbitrary <*> listOf1 arbitrary)
      ]
    frequency
      [ (10, return term)
      , (1, TRep term
          <$> genPositiveInteger
          <*> oneof [Just <$> genPositiveInteger, return Nothing]
        )
      ]


instance PrettyPrint RegTerm where
  pp = \case
    TChar c -> c:""
    TScope r -> concat ["(", pp r, ")"]
    TEscaped e -> pp e
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

data REscaped where
  Escaped :: Char   -> REscaped
  Latin   :: String -> REscaped
  Unicode :: String -> REscaped
  Control :: Char   -> REscaped
  deriving (Show, Eq, Read)

instance Arbitrary REscaped where
  arbitrary = oneof [ Escaped <$> suchThat genValidChar (not . (`elem` "cuxCUX"))
                    , Latin   <$> genHex 2
                    , Unicode <$> genHex 4
                    , Control <$> genLetterChar
                    ]

instance PrettyPrint REscaped where
  pp = \case
    Escaped c  -> '\\':c:""
    Latin   hx -> "\\x" ++ hx
    Unicode hx -> "\\u" ++ hx
    Control hx -> '\\':'c':hx:""

data CharsetItem
  = SSpan Char Char
  | SChar Char
  deriving (Eq, Show, Read)

instance PrettyPrint CharsetItem where
  pp = \case
    SSpan start end -> concat [start:"", "-", end:""]
    SChar c -> c:""

instance Arbitrary CharsetItem where
  arbitrary = oneof
    [ SChar <$> genValidChar
    , uncurry SSpan <$> genCharRange
    ]

genCharRange :: Gen (Char, Char)
genCharRange = do
  let replaces = [ (40, 38), (41, 42), (91, 92), (93, 94), (122, 121)
                 , (123, 121), (124, 125)
                 , (ord '*', 178)
                 , (ord '-', 177)
                 , (ord '^', 176) 
                 ]
      invalidSwap x = case find ((== x) . fst) replaces of 
        Nothing      -> x
        Just (_, rp) -> rp

  i <- genValidChar
  bimap (chr . invalidSwap . max 32 ) (chr . invalidSwap . min 126)
    <$> ((,) <$> choose (32, ord i) <*> choose (ord i, 90))