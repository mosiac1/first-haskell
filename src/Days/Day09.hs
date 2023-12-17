module Days.Day09
  ( runDay
  ) where

import qualified Program.RunDay as R (Day, runDay)
import Util.Util

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: String -> Input
inputParser = parseOases

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . map oasisPredict . map reverse

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map oasisPredict

parseOases :: String -> [[Int]]
parseOases = map (map parseInt . words) . lines

oasisNextHistory :: Num a => [a] -> [a]
oasisNextHistory [] = []
oasisNextHistory [_] = []
oasisNextHistory (x1:x2:xs) = (x1 - x2 : oasisNextHistory (x2 : xs))

oasisAllHistories :: (Eq a, Num a) => [a] -> [[a]]
oasisAllHistories initHist =
  case all ((==) 0) initHist of
    True -> [initHist]
    False -> [initHist] ++ oasisAllHistories (oasisNextHistory initHist)

oasisPredict :: (Eq a, Num a) => [a] -> a
oasisPredict = sum . map head . oasisAllHistories
