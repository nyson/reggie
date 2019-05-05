{-# LANGUAGE LambdaCase, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies, RecordWildCards, GeneralizedNewtypeDeriving #-}
module Text.Reggie.Lexer where

import Prelude as P hiding (lex)
import Data.Char (isAlphaNum)

import Test.QuickCheck
import Text.Reggie.Prelude
import Data.Proxy
import Data.Bifunctor
import qualified Text.Megaparsec as M
import qualified Data.List.NonEmpty as NE
import qualified Data.List as DL

data WithPos a = WithPos
  { startPos :: M.SourcePos
  , endPos   :: M.SourcePos
  , tokenVal :: a
  } deriving (Eq, Ord, Show)

getLexemes :: RegLexStream -> [RegLexeme]
getLexemes = map tokenVal . unRegLexStream

newtype RegLexStream = RegLexStream { unRegLexStream :: [WithPos RegLexeme]}
  deriving (Eq, Ord, Show, Semigroup, Monoid)

instance M.Stream RegLexStream where
  type Token RegLexStream  = WithPos RegLexeme
  type Tokens RegLexStream = [WithPos RegLexeme]
  tokenToChunk  Proxy x  = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy    = id
  chunkLength   Proxy    = length
  chunkEmpty    Proxy    = null
  take1_ (RegLexStream [])     = Nothing
  take1_ (RegLexStream (t:ts)) = Just (t, RegLexStream ts)
  takeN_ n (RegLexStream s)
    | n <= 0    = Just ([], RegLexStream s)
    | null s    = Nothing
    | otherwise = Just . second RegLexStream $ DL.splitAt n s
  takeWhile_ f (RegLexStream s) = second RegLexStream $ DL.span f s
  showTokens Proxy = DL.intercalate ", " . NE.toList . fmap (show . tokenVal)
  reachOffset o pst@M.PosState {..} =
    case P.drop (o - pstateOffset) (unRegLexStream pstateInput) of
      [] ->
        ( pstateSourcePos
        , "<missing input>"
        , pst { M.pstateInput = mempty }
        )
      s@(x:_) ->
        ( startPos x
        , "<missing input>"
        , pst { M.pstateInput = RegLexStream s }
        )


metas :: String
metas = "0tnvfrsSdDwW"

operators :: String
operators = "*+?|^$"

data RegLexeme
  = RLit Char
  | RBackslash
  | RLatin String
  | RUnicode String
  | RControl Char
  | RMeta Char
  deriving (Eq, Show, Ord)

instance Arbitrary RegLexeme where
  arbitrary = frequency
    [ (100,  RLit <$> alphaNum)
    , (1,    return RBackslash)
    , (10,   RLatin <$> genHex 2)
    , (10,   RUnicode <$> genHex 4)
    , (10,   RControl <$> elements (['a' .. 'z'] ++ ['A' .. 'Z']))
    , (10,   RMeta <$> elements metas)
    ]
    where
      alphaNum = elements $ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['A' .. 'Z']

instance PrettyPrint [RegLexeme] where
  pp = concatMap t
    where t = \case
            RLit c -> [c]
            RBackslash -> "\\\\"
            RLatin hexval -> "\\x" ++ hexval
            RUnicode hexval -> "\\u" ++ hexval
            RControl con -> "\\c" ++ [con]
            RMeta c -> "\\" ++ [c]

dummySP :: M.SourcePos
dummySP = M.SourcePos "nan" (M.mkPos 1) (M.mkPos 1)

lex :: String -> Either String RegLexStream
lex = fmap (RegLexStream . map (\a -> WithPos dummySP dummySP a)) . innerLex

innerLex :: String -> Either String [RegLexeme]
innerLex [] = Right []
innerLex cs = case cs of
  ('\\':'\\':cs')
    -> (RBackslash :) <$> innerLex cs'
  ('\\':c:cs') -> case c of
    'x' | length cs' >= 2
        -> (RLatin (take 2 cs')  :) <$> innerLex (drop 2 cs')
    'u' | length cs' >= 4
        -> (RUnicode (take 4 cs'):) <$> innerLex (drop 4 cs')
    'c' | length cs' >= 1
        -> (RControl (head cs')  :) <$>  innerLex (tail cs')
    ch  | isLitOrMeta ch
        -> (RMeta ch:) <$> innerLex cs'
    _ -> Left $ "'\\" ++ [c] ++ "' is not a valid literal or meta character"
  (c:cs') -> (RLit c:) <$> innerLex cs'
  _ -> Left $ "I don't know how to parse '" ++ cs ++ "'"
  where isLitOrMeta :: Char -> Bool
        isLitOrMeta c = all ($ c) [isAlphaNum, (`elem` metas)]

