{-# LANGUAGE LambdaCase, TypeSynonymInstances, FlexibleInstances #-}
module Text.Reggie.Parser where

import Text.Reggie.AST
import Text.Reggie.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import Data.Functor  (($>))
import Data.Void
import Data.List     (intercalate)
import Control.Monad (when)

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Text.Megaparsec as M

import Control.Monad.Combinators.Expr

pFail :: String -> Parser ()
pFail = fancyFailure . S.singleton . ErrorFail

type Parser = Parsec Void String

prettifyErrors :: ParseErrorBundle String Void -> NonEmpty String
prettifyErrors errors = showParseError <$> bundleErrors errors
  where
    showParseError :: ParseError String Void -> String
    showParseError (TrivialError a b c)
      = let ppErrItem = \case
              EndOfInput  -> "end of input"
              Label chs   -> "label '" ++ NE.toList chs ++ "'"
              Tokens toks -> "tokens " ++ (intercalate ", " . map (\t -> '\'':t:"'") $ NE.toList toks)
            combErrorSet = intercalate ", " . map (\t -> '\'':t:"'")
                           . concatMap (\case Tokens tks -> NE.toList tks
                                              _ -> []) . S.toList
        in show a ++ ": "
           ++ case b of
                Just ts -> "Unexpected " ++ ppErrItem ts ++ "; "
                Nothing -> "No input; "
           ++ "expected " ++ combErrorSet c
    showParseError (FancyError a b) = show a ++ ": " ++ show b

unsafeParse :: String -> Regex
unsafeParse str = case parse str of
  Left  err -> error err
  Right res -> res

parse :: String -> Either String Regex
parse input = first (concat . prettifyErrors) $ M.parse regex "" input

regex :: Parser Regex
regex = makeExprParser (Single <$> stream) ops
  where
    -- ops :: [[ Operator Parser Regex ]]
    ops = [[ binary "|" regconcat ]]
    binary :: String -> (a -> a -> a) -> Operator Parser a
    binary n f = InfixL $ f <$ L.symbol (return ()) n
    regconcat r1 r2 = Union r1 r2


stream :: Parser RegexStream
stream = RegexStream <$> many (try term)

regexChar :: Parser Char
regexChar = do
    ch <- printChar
    when (ch `elem` "[(|)]") (pFail $ "tried to parse nonescaped reserved char " ++ show ch)
    return ch

term :: Parser RegTerm
term = repetitions =<< choice
  [ parens <?> "Scoped regular expression"
  , charsetTerm <?> "Charset term"
  , (TEscaped <$> escaped) <?> "Escaped character"
  , TChar <$> regexChar
  ]

parens :: Parser RegTerm
parens = TScope <$> (char '(' *> regex <* char ')')

charsetTerm :: Parser RegTerm
charsetTerm = label "charset" $ choice
  [ TCharset True  <$> cst (string "[^")
  , TCharset False <$> cst (char '[')
  ]
  where cst prefix = prefix *> many charsetItem <* char ']'

hex :: Int -> Parser String
hex i = label (show i ++ " hexadecimal characters") $ hx i
 where hx 0   = return "" :: Parser String
       hx len = hexDigitChar >>= \h -> fmap (h:) (hx (len - 1))

escaped :: Parser REscaped
escaped = label "escaped char" $ char '\\' *> choice
  [ Unicode <$> (char' 'u' *> hex 4)
  , Latin   <$> (char' 'x' *> hex 2)
  , Control <$> (char' 'c' *> letterChar)
  , Escaped <$> printChar
  ]

repetitions :: RegTerm -> Parser RegTerm
repetitions rterm = label "repetition (*,+ and {n,m})" $ choice
  [ char '*' $> TRep rterm 0 Nothing
  , char '+' $> TRep rterm 1 Nothing
  , TRep rterm
    <$> (char '{' *> space *> L.decimal <* space)
    <*> (  char ','
        *> space
        *> choice [try $ Just <$> L.decimal, pure Nothing]
        <* space
        <* char '}'
        )
  , return rterm
  ]

charsetItem :: Parser CharsetItem
charsetItem = choice
  [ try $ SSpan <$> regexChar <*> (char '-' *> regexChar)
  , try $ SChar <$> regexChar
  ]
