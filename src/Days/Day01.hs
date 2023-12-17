module Days.Day01
  ( runDay
  ) where

{- ORMOLU_DISABLE -}
import Data.Maybe
import Util.Util (headAndTail)

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)
import qualified Program.RunDay as R (Day, runDay)

{- ORMOLU_ENABLE -}
runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: String -> [String]
inputParser = lines

------------ TYPES ------------
type Input = [String]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA =
  sum . (map $ (\(x, y) -> digitToInt x * 10 + digitToInt y) . firstLastDigits)

------------ PART B ------------
partB :: Input -> OutputB
partB =
  sum .
  (map $
   (\(x, y) -> fromJust x * 10 + fromJust y) .
   headAndTail . filter isJust . parseDigSpel)

firstLastDigits :: [Char] -> (Char, Char)
firstLastDigits = headAndTail . filter isDigit

matchSpel :: String -> Maybe Int
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
