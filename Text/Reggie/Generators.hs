module Text.Reggie.Generators where 

import Control.Applicative (liftA2)
import Test.QuickCheck


hex :: Int -> Gen String
hex size
  | size <= 0 = return ""
  | otherwise = liftA2 (:) (elements hexChars) (hex $ size - 1)
  where hexChars = ['a' .. 'f'] ++ ['A' .. 'F'] ++ ['0' .. '9']
