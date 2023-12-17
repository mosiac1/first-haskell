module Days.Day06
  ( runDay
  ) where

import qualified Program.RunDay as R (Day, runDay)
import Util.Util

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: String -> Input
inputParser = map (map parseInt . words . drop 11) . lines

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA =
  product .
  map ((\(l, r) -> r - l + 1) . getRaceWinInterv) .
  (\(times, dists) -> zip times dists) . headAndTail

------------ PART B ------------
partB :: Input -> OutputB
partB =
  (\(l, r) -> r - l + 1) .
  getRaceWinInterv . headAndTail . map (foldl prependDigit 0)

-- pressTime * (raceTime - pressTime) = best
-- | - pressTime ^ 2 + pressTime * raceTime - best = 0
-- | -x^2 + x * raceTime - best = 0
-- | delta = raceTime ^ 2 - 4 * best
-- | x = (-b +- sqrt(raceTime ^ 2 - 4 * best)) / -2
-- | x = (b +- sqrt(raceTime ^ 2 - 4 * best)) / 2
-- | x = (raceTime +- sqrt(raceTime ^ 2 - 4 * best)) / 2
-- take left rounded up, right rounded down
-- adding/subtracting 0.001 to account for equality cases
getRaceWinInterv :: (Int, Int) -> (Int, Int)
getRaceWinInterv (timeInt, bestDistInt) =
  let time = fromIntegral timeInt
      bestDist = fromIntegral bestDistInt
      delta = sqrt (time ^ 2 - 4 * bestDist)
      lowX = (time - delta) / 2
      highX = (time + delta) / 2
   in (ceiling (lowX + 0.001), floor (highX - 0.001))
