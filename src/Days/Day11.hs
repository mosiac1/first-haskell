module Days.Day11
  ( runDay
  ) where

import Data.Array (Array, (!))
import qualified Data.Array as Array
import qualified Data.Set as Set
import qualified Program.RunDay as R (Day, runDay)
import Util.Util

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: String -> Input
inputParser = parseGalaxyMap

------------ TYPES ------------
type Input = GalaxyMap

type OutputA = Int

type OutputB = Int

data GalaxyMapEl
  = Galaxy
  | Space
  deriving (Show, Eq)

data GalaxyMap = GalaxyMap
  { galaxyMap :: Array (Int, Int) GalaxyMapEl
  } deriving (Show)

------------ PART A ------------
partA :: Input -> OutputA
partA = galaxySumDistances 1

------------ PART B ------------
partB :: Input -> OutputB
partB = galaxySumDistances 999999

parseGalaxyMapEl :: Char -> GalaxyMapEl
parseGalaxyMapEl s =
  case s of
    '.' -> Space
    '#' -> Galaxy
    c -> error $ "Invalid GalaxyMapEl " ++ show c

parseGalaxyMap :: String -> GalaxyMap
parseGalaxyMap s =
  let sLines = lines s
      height = length sLines
      width = length $ head sLines
   in GalaxyMap $
      Array.array ((1, 1), (height, width)) $
      concat $
      map
        (\(posX, elems) -> map (\(posY, _elem) -> ((posX, posY), _elem)) elems) $
      zip [1 .. height] $ map (zip [1 .. width] . map parseGalaxyMapEl) sLines

expandedRowsCols :: GalaxyMap -> (Set.Set Int, Set.Set Int)
expandedRowsCols _galaxyMap@(GalaxyMap galaxyArr) =
  let (_, (width, height)) = Array.bounds galaxyArr
      expandedRows =
        filter
          (\posX ->
             all ((==) Space) $
             map (\posY -> galaxyArr ! (posX, posY)) [1 .. width])
          [1 .. height]
      expandedCols =
        filter
          (\posY ->
             all ((==) Space) $
             map (\posX -> galaxyArr ! (posX, posY)) [1 .. height])
          [1 .. width]
   in (Set.fromList expandedRows, Set.fromList expandedCols)

galaxyDistance ::
     Set.Set Int -> Set.Set Int -> Int -> (Int, Int) -> (Int, Int) -> Int
galaxyDistance expandedRows expandedCols multiplier (g1X, g1Y) (g2X, g2Y) =
  let lowX = min g1X g2X
      highX = max g1X g2X
      xDist =
        highX - lowX + multiplier * setCountInRange expandedRows lowX highX
      lowY = min g1Y g2Y
      highY = max g1Y g2Y
      yDist =
        highY - lowY + multiplier * setCountInRange expandedCols lowY highY
   in xDist + yDist

galaxySumDistances multiplier _galaxyMap@(GalaxyMap galaxyArr) =
  let (_, (width, height)) = Array.bounds galaxyArr
      (expandedRows, expandedCols) = expandedRowsCols _galaxyMap
      galaxies =
        [ (i, j)
        | i <- [1 .. height]
        , j <- [1 .. width]
        , galaxyArr ! (i, j) == Galaxy
        ]
      galaxyPairs = allPairs galaxies
   in sum $
      map
        (uncurry $ galaxyDistance expandedRows expandedCols multiplier)
        galaxyPairs
