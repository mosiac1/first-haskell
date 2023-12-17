module Days.Day02
  ( runDay
  ) where

{- ORMOLU_DISABLE -}
import Util.Util (parseInt)

import Data.List.Split (splitOn)
import qualified Program.RunDay as R (Day, runDay)
import Text.Regex.Posix

{- ORMOLU_ENABLE -}
runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: String -> Input
inputParser = map parseCubeGame . lines

------------ TYPES ------------
type Input = [CubeGame]

type OutputA = Int

type OutputB = Int

data CubeGrab = CubeGrab
  { green :: Int
  , blue :: Int
  , red :: Int
  } deriving (Show)

data CubeGame = CubeGame
  { gameId :: Int
  , grabs :: [CubeGrab]
  } deriving (Show)

------------ PART A ------------
partA :: Input -> OutputA
partA = sumValidCubeGames

------------ PART B ------------
partB :: Input -> OutputB
partB = sumMinFeasibleCubeGrabPowers

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
