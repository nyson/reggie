module DSLTest where

import Text.Reggie.DSL
import Test.QuickCheck

prop_randomChar :: Char -> Property
prop_randomChar c = c === toChar (fromChar c)
