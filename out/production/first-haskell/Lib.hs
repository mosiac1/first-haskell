module Lib
  ( solveDay1P1
  , parseDigSpel
  ) where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)

parseInt :: String -> Integer
parseInt = read

-- onetwo1threeight5
-- 1  2  13   8    5
matchSpel :: [Char] -> Maybe Int
matchSpel s =
  if isPrefixOf "one" s
    then Just 1
    else if isPrefixOf "two" s
           then Just 2
           else if isPrefixOf "three" s
                  then Just 3
                  else if isPrefixOf "four" s
                         then Just 4
                         else if isPrefixOf "five" s
                                then Just 5
                                else if isPrefixOf "six" s
                                       then Just 6
                                       else if isPrefixOf "seven" s
                                              then Just 7
                                              else if isPrefixOf "eight" s
                                                     then Just 8
                                                     else if isPrefixOf "nine" s
                                                            then Just 9
                                                            else Nothing

parseDigSpel :: [Char] -> [Maybe Int]
parseDigSpel [] = [Nothing]
parseDigSpel (x:xs) =
  [ if (isDigit x)
      then Just (digitToInt x)
      else matchSpel ([x] ++ xs)
  ] ++
  parseDigSpel xs

solveDay1P1 :: String -> String
solveDay1P1 = show . sum . (map $ parseInt . firstLastDigits) . (splitOn "\n")
  where
    firstLastDigits :: [Char] -> [Char]
    firstLastDigits = headAndTail . filter isDigit
    headAndTail :: [a] -> [a]
    headAndTail [] = error "headAndTail: Error no elements"
    headAndTail [x] = [x, x]
    headAndTail (x:xs) = [x, last xs]
