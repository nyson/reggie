{-# LANGUAGE LambdaCase, TypeSynonymInstances, FlexibleInstances #-}
module Text.Reggie.Parser where

import Text.Reggie.AST
import Text.Reggie.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import Data.List (intercalate)
import Debug.Trace (trace)

import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Text.Megaparsec as M

import Control.Monad.Combinators.NonEmpty
import Control.Monad.Combinators.Expr


type Parser = Parsec String String

instance ShowErrorComponent String where
  showErrorComponent = id
  errorComponentLen  = length

prettifyErrors :: ParseErrorBundle String String -> NonEmpty String
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

parse :: String -> Either (NonEmpty String) Regex
parse input = first prettifyErrors $ M.parse regex "" input

regex :: Parser Regex
regex = makeExprParser (Regex . (:[]) <$> stream) ops
  where 
    ops :: [[ Operator Parser Regex ]]
    ops = [[ binary "|" regconcat ]]
    binary :: String -> (a -> a -> a) -> Operator Parser a
    binary n f = InfixL $ f <$ L.symbol space n
    regconcat (Regex a) (Regex b) = Regex $ a ++ b

          
stream :: Parser RegexStream
stream = RegexStream <$> many term

term :: Parser RegTerm
term = repetitions =<< choice
  [ try $ charsetTerm 
  , try $ TEscaped <$> escaped
  , TChar <$> alphaNumChar
  ]

charsetTerm :: Parser RegTerm
charsetTerm = choice 
  [ TCharset True  <$> cst (string "^[")
  , TCharset False <$> cst (char '[')
  ]
  where cst prefix = prefix *> many charsetItem <* char ']'

hex :: Int -> Parser String
hex 0   = return ""
hex len = hexDigitChar >>= \h -> fmap (h:) (hex (len - 1))

escaped :: Parser REscaped
escaped = char '\\' *> choice 
  [ Unicode <$> (char' 'u' *> hex 4)
  , Latin   <$> (char' 'x' *> hex 2)
  , Control <$> (char' 'c' *> letterChar)
  , Escaped <$> printChar
  ]

repetitions :: RegTerm -> Parser RegTerm
repetitions term = choice 
  [ char '*' *> return (TRep term 0 Nothing)
  , char '+' *> return (TRep term 1 Nothing)
  , TRep term 
    <$> (char '{' *> space *> L.decimal <* space)
    <*> (  char ','
        *> space 
        *> choice [try $ Just <$> L.decimal, pure Nothing] 
        <* space 
        <* char '}'
        )  
  , return term
  ]
  
charsetItem :: Parser CharsetItem
charsetItem = choice
  [ try $ SSpan <$> alphaNumChar <*> (char '-' *> alphaNumChar)
  , SChar <$> alphaNumChar
  ]



