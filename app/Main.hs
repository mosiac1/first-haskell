module Main
  ( main
  ) where

import Lib
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust)
import Data.Char (digitToInt, isDigit)

-- Day 1, Part 2 Solution
solveDay1P2 :: [Char] -> String
solveDay1P2 =
  show .
  sum .
  (map $
   (\(x, y) -> fromJust x * 10 + fromJust y) .
   headAndTail . filter isJust . parseDigSpel) .
  (splitOn "\n")

-- Day 1, Part 1 Solution
solveDay1P1 :: [Char] -> String
solveDay1P1 =
  show .
  sum .
  (map $ (\(x, y) -> digitToInt x * 10 + digitToInt y) . firstLastDigits) .
  (splitOn "\n")
  where
    firstLastDigits :: [Char] -> (Char, Char)
    firstLastDigits = headAndTail . filter isDigit

main :: IO ()
main = interact solveDay1P2