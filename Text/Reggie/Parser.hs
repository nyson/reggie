{-# LANGUAGE LambdaCase, TypeFamilies, RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}
module Text.Reggie.Parser (parse, regexTerm) where

import Prelude as P hiding (lex)
import Control.Monad (void)
import Data.Bifunctor
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.NonEmpty as NE
import Control.Monad.Combinators.Expr

import Data.List(intercalate)
import qualified Text.Megaparsec as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import Text.Reggie.DSL
import Text.Reggie.Prelude

import Debug.Trace

type Parser = Parsec String String

-- TODO: parse "$[a-z]*" becomes 0*, why?

instance ShowErrorComponent String where
  showErrorComponent = id
  errorComponentLen  = length

prettifyErrors :: ParseErrorBundle String String -> NE.NonEmpty String
prettifyErrors errors = fmap showParseError $ bundleErrors errors
  where
    showParseError :: ParseError String String -> String
    showParseError (TrivialError a b c)
      = let ppErrItem = \case
              EndOfInput  -> "end of input"
              Label chs   -> "label '" ++ NE.toList chs ++ "'"
              Tokens toks -> "tokens " ++ (intercalate ", " . map (\t -> '\'':t:"'") $ NE.toList toks)
            combErrorSet = intercalate ", " . map (\t -> '\'':t:"'") . concatMap (\case Tokens tks -> NE.toList tks
                                                                                        other -> trace (show other) []) . S.toList
        in show a ++ ": "
           ++ case b of
                Just ts -> "Unexpected " ++ ppErrItem ts ++ "; "
                Nothing -> "No input; "
           ++ "expected " ++ combErrorSet c
    showParseError (FancyError a b) = show a ++ ": " ++ show b

parse :: String -> Either (NE.NonEmpty String) Reggex
parse input = first prettifyErrors $ M.parse outerRegex "" input

maybeP :: Parser a -> Parser Bool
maybeP p = choice [ try p *> return True
                  , return False]

outerRegex :: Parser Reggex
outerRegex = do
  start <- maybeP (char '^')
  re <- regex
  end <- maybeP (char '$')
  return $ case border start end of
    Nothing -> Reggex re
    Just b -> Enclosed b re

regex :: Parser RegexTerm
regex = makeExprParser regexTerm operators

regexTerm :: Parser RegexTerm
regexTerm = choice [ rstring
                   , parens regex
                   , RCharSet <$> charset
                   ]
            >>= range

-- Default symbol consumber
symbol :: MonadParsec e s m => Tokens s -> m (Tokens s)
symbol = L.symbol (return ())

-- Helper for postfix operators
postfix :: String -> (RegexTerm -> RegexTerm) -> Operator Parser RegexTerm
postfix name node = Postfix (node <$ symbol name)

-- Regex operators
operators :: [[Operator Parser RegexTerm]]
operators = [[ postfix "*" Star
             , postfix "?" Greedy
             , postfix "+" Plus
             ]
            ]

charset :: Parser CharSet
charset = between (char '[') (char ']') (choice cs)
    where cs = [ try $ NCharSet <$> (char '^' *> NE.some charsetItem)
               , CharSet  <$> NE.some charsetItem
               ]

charsetItem :: Parser CharSetItem
charsetItem = choice
  [ try $ Span <$> lit <*> (char '-' >> lit)
  , RCSLit <$> lit
  ]

range :: RegexTerm -> Parser RegexTerm
range re = choice
  [ try $ r
  , return re
  ]
  where
    r = do start <- char '{'
                    *> spaced integer
                    <* char ','
           choice [ try    $ Length  re start <$> spaced integer
                  , return $ AtLeast re start]
               <* spaced (char '}')

spaced :: Parser a -> Parser a
spaced p = many (char ' ') *> p <* many (char ' ')

integer :: Parser Int
integer = L.decimal

rstring :: Parser RegexTerm
rstring = String <$> NE.some lit

escaped :: Char -> Parser ()
escaped c = void (char '\\' >> char c)

hex :: Int -> Parser String
hex 0 = return ""
hex len = hexDigitChar >>= \h -> fmap (h:) (hex (len - 1))

lit :: Parser ReggexChar
lit = choice [ try $ Unicode <$> (escaped 'u' *> hex 4)
             , try $ Latin   <$> (escaped 'x' *> hex 2)
             , try $ Control <$> (escaped 'c' *> letterChar)
             , char '.'    *> return Dot
             , try $ escaped 'n' *> return Newline
             , try $ escaped '0' *> return Null
             , try $ escaped 't' *> return Tab
             , try $ escaped 'v' *> return VTab
             , try $ escaped 'f' *> return FormFeed
             , try $ escaped 'r' *> return CarReturn
             , try $ escaped 's' *> return Whitespace
             , try $ escaped 'd' *> return Digit
             , try $ escaped 'w' *> return Word
             , try $ escaped 'b' *> return Backspace
             , try $ AlphaNum <$> char ' '
             , AlphaNum <$> alphaNumChar
             ]

parens :: Parser a -> Parser a
parens   = between (symbol "(") (symbol ")")
