{-# LANGUAGE TypeApplications #-}
module Main (main) where

import System.Environment
import Text.Reggie.Prelude (pp)
import Text.Reggie
import Text.Reggie.AST
import Text.Read (readMaybe)

main :: IO ()
main = getArgs >>= pArg

pArg :: [String] -> IO ()
pArg ("-p":r:_) = case parse r of
    Left e -> print e
    Right s -> mapM_ putStrLn
      [ "read '"++ r ++ "'..."
      , "parsed as '"++ show s ++ "'"
      , "pretty-printed as '"++ pp s ++"'"]
pArg ("-r":re:_) = case readMaybe @Regex re of
  Just r  -> putStrLn $ pp r
  Nothing -> putStrLn $ "Couldn't parse expression: '"++ re ++"'"
pArg args = putStrLn $ "Arguments not recognized " ++ unwords args
