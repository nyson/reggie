module ParserTest where

import Test.QuickCheck
import Test.QuickCheck.Property
import Text.Reggie.Prelude
import Text.Reggie.DSL
import Data.Either
import Text.Reggie.Parser as R
import Text.Reggie.Lexer as L

prop_circularLex :: [RegLexeme] -> Property
prop_circularLex rls = let l = L.getLexemes <$> (L.lex (pp rls))
                           r = Right rls
                       in l === r

prop_circularParse :: Reggex -> Property
prop_circularParse rx
  = let reparsed =  R.parse (pp rx)
    in classify (isRight reparsed) "successful parse"
       $ classify (isLeft reparsed)  "erroneous parse"
       $ case reparsed of Right rp -> rp ===  rx
                          Left err -> error $ show err


