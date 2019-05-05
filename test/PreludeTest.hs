module PreludeTest where

import Test.QuickCheck
import Text.Reggie.Prelude

prop_condOp :: Bool -> String -> String -> Property
prop_condOp c a b = c ? a € b === if c then a else b
