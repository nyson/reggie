{-# LANGUAGE GADTs, LambdaCase #-}
module Text.Reggie.AST where

import Test.QuickCheck
import Text.Reggie.Prelude
import Data.List (intercalate)

-- A disjoint stream of regexes
newtype Regex = Regex [RegexStream]
  deriving (Show, Read)

instance Eq Regex where
  r1 == r2 = cleanEmpty r1 == cleanEmpty r2
    where cleanEmpty (Regex str) = filter (\(RegexStream rstr) -> rstr /= []) $ str

instance Arbitrary Regex where
  arbitrary = Regex <$> listOf arbitrary

instance PrettyPrint Regex where
  pp (Regex rstr) = intercalate "|" . map pp $ rstr

-- A conjunctive stream of regex terms
newtype RegexStream = RegexStream [RegTerm]
  deriving (Show, Eq, Read)

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
    term <- oneof [ TScope   <$> arbitrary
                  , TChar    <$> genValidChar 
                  , TEscaped <$> arbitrary
                  , TCharset <$> arbitrary <*> arbitrary
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
      [ if negated then "^" else mempty
      , "["
      , concatMap pp csis
      , "]"
      ]

data REscaped where
  Escaped :: Char   -> REscaped
  Latin   :: [Char] -> REscaped
  Unicode :: [Char] -> REscaped
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
    [ SSpan <$> genValidChar <*> genValidChar
    , SChar <$> genValidChar
    ]
