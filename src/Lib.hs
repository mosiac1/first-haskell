module Lib
  ( parseInt
  , headAndTail
  , matchSpel
  , parseDigSpel
  , parseCubeGame
  , sumValidCubeGames
  , sumMinFeasibleCubeGrabPowers
  ) where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Text.Regex.Posix

-- Library Function
parseInt :: String -> Int
parseInt = read

headAndTail :: [a] -> (a, a)
headAndTail [] = error "headAndTail: Error no elements"
headAndTail [x] = (x, x)
headAndTail (x:xs) = (x, last xs)

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

-- Day 2 --
data CubeGrab = CubeGrab
  { green :: Int
  , blue :: Int
  , red :: Int
  } deriving (Show)

data CubeGame = CubeGame
  { gameId :: Int
  , grabs :: [CubeGrab]
  } deriving (Show)

parseCubeGrab :: String -> CubeGrab
parseCubeGrab s =
  CubeGrab
    { blue = matchIntWithSuffix "blue"
    , red = matchIntWithSuffix "red"
    , green = matchIntWithSuffix "green"
    }
  where
    matchIntWithSuffix :: String -> Int
    matchIntWithSuffix pref =
      case s =~ ("([0-9]+) " ++ pref) :: [[String]] of
        ([_, matchB]:_) -> parseInt matchB
        _ -> 0

parseCubeGame :: String -> CubeGame
parseCubeGame s =
  let parsedId =
        case s =~ "Game ([0-9]+):" :: [[String]] of
          ([_, matchedGameId]:_) -> parseInt matchedGameId
          _ -> error ("Failed to get game id from " ++ s)
      parsedGrabs =
        map parseCubeGrab $ splitOn ";" $ tail $ dropWhile (/= ':') s
   in CubeGame {gameId = parsedId, grabs = parsedGrabs}

isValidCubeGrab :: CubeGrab -> Bool
isValidCubeGrab (CubeGrab g b r) = r <= 12 && g <= 13 && b <= 14

isValidCubeGame :: CubeGame -> Bool
isValidCubeGame (CubeGame _ g) = all isValidCubeGrab g

sumValidCubeGames :: [CubeGame] -> Int
sumValidCubeGames = sum . (map gameId) . (filter isValidCubeGame)

minFeasibleCubeGrab :: [CubeGrab] -> CubeGrab
minFeasibleCubeGrab =
  foldr1
    (\(CubeGrab g1 b1 r1) (CubeGrab g2 b2 r2) ->
       CubeGrab {blue = max b1 b2, red = max r1 r2, green = max g1 g2})

cubeGrabPower :: CubeGrab -> Int
cubeGrabPower (CubeGrab g b r) = g * b * r

sumMinFeasibleCubeGrabPowers :: [CubeGame] -> Int
sumMinFeasibleCubeGrabPowers =
  sum . map (cubeGrabPower . minFeasibleCubeGrab . grabs)
