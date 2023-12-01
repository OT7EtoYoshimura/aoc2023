{-# LANGUAGE OverloadedStrings #-}
module Day1 where

import Data.Char
import Data.List
import Data.Tuple.Extra

import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = print . both (sum . map solve) . (id &&& map repAll) . T.lines =<< T.readFile "in/d1bigboy"

solve :: T.Text -> Integer
solve = read . uncurry mappend . both singleton . (T.head &&& T.last) . T.filter isDigit

repAll :: T.Text -> T.Text
repAll line = rep patterns line where
  rep [] line     = line
  rep (x:xs) line = rep xs $ uncurry T.replace x line
  patterns = [ ("one"  , "o1e"), ("two" ,  "t2o"), ("three", "th3e")
             , ("four" , "4"  ), ("five" , "5e" ), ("six"  , "6"   )
             , ("seven", "7n" ), ("eight", "e8t"), ("nine" , "9e"  ) ]
