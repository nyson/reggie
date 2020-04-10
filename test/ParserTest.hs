module ParserTest where

import Test.QuickCheck
import Test.QuickCheck.Property
import Text.Reggie.Prelude
import Text.Reggie.RegexGenerator
import Text.Reggie.Match
import Text.Reggie.AST
import Data.Either
import Text.Reggie.Parser as R

prop_circularParse :: Regex -> Property
prop_circularParse rx
  = let reparsed =  R.parse (pp rx)
    in classify (isRight reparsed) "successful parse"
       $ classify (isLeft reparsed)  "erroneous parse"
       $ case reparsed of Right rp -> rp ===  rx
                          Left err -> error $ show err

-- prop_circularMatch :: Regex -> Property
-- prop_circularMatch reg = case eGen of
--   Left _err -> discard
--   Right gen -> forAll gen $ \s -> (match_ reg s, s, reg) === (True, s, reg)
--   where eGen = mkGenRegex reg

