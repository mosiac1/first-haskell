module Days.Day08
  ( runDay
  ) where

import Data.List.Split (splitOn)
{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Program.RunDay as R (Day, runDay)
import Text.Regex.Posix
import Util.Util

{- ORMOLU_ENABLE -}
runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: String -> Input
inputParser = parseDesertMap

------------ TYPES ------------
type Input = DesertMap

type OutputA = Int

type OutputB = Int

data DesertDirection
  = DesertLeft
  | DesertRight
  deriving (Show)

data DesertMap = DesertMap
  { direction :: [DesertDirection]
  , paths :: Map String (String, String)
  } deriving (Show)

------------ PART A ------------
partA :: Input -> OutputA
partA = walkDesertMap

------------ PART B ------------
partB :: Input -> OutputB
partB = walkDesertMapAtoZ

parseDesertMap :: String -> DesertMap
parseDesertMap s =
  let (directionChars, pathsChars) = headAndTail $ splitOn "\n\n" s
      _paths = Map.fromList $ map parseDesertPath $ lines pathsChars
      _direction = map parseDesertDirection directionChars
   in (DesertMap _direction _paths)
  where
    parseDesertPath _s =
      case _s =~ "(.*) = \\((.*), (.*)\\)" :: [[String]] of
        ([_, _source, _dirLeft, _dirRight]:_) ->
          (_source, (_dirLeft, _dirRight))
        _ -> error $ "Error unpacking map - " ++ _s
    parseDesertDirection _s =
      case _s of
        'L' -> DesertLeft
        'R' -> DesertRight
        _ -> error $ "Error parsing direction"

advanceLocation ::
     Map String (String, String) -> DesertDirection -> String -> String
advanceLocation desertPaths curDirection curLocation =
  let pathChoices = fromJust $ Map.lookup curLocation desertPaths
   in case curDirection of
        DesertLeft -> fst pathChoices
        DesertRight -> snd pathChoices

walkDesertRec ::
     Map String (String, String)
  -> String
  -> (String -> Bool)
  -> [DesertDirection]
  -> (Int, String)
walkDesertRec _ _ _ [] = error "Exhausted directions"
walkDesertRec desertPaths curLocation endChecker (curDirection:directions) =
  case endChecker curLocation of
    True -> (0, curLocation)
    False ->
      let (recDist, recEnd) =
            walkDesertRec
              desertPaths
              (advanceLocation desertPaths curDirection curLocation)
              endChecker
              directions
       in (1 + recDist, recEnd)

walkDesertMap :: DesertMap -> Int
walkDesertMap dm =
  fst $ walkDesertRec (paths dm) "AAA" ((==) "ZZZ") (cycle $ direction dm)

-- This makes use a very important pattern in the input that is not specified in the problem text:
-- For every node ending with A it reaches a node ending with Z in N steps, then the Z node loops back
--  to itself after N more steps, not passing through any other Z nodes.
-- In short **A -> **Z takes N, then **Z -> **Z takes N again, so **Z is reached every N steps.
-- Apply LCM to get the sync.
-- The general solution is WAY more complicated and not covered here.
findDesertAToZLengths :: DesertMap -> [(Int, Int, String, String)]
findDesertAToZLengths (DesertMap _direction _paths) =
  let startPoints = filter ((==) 'A' . last) $ Map.keys _paths
      distanceStartEndLoop = map startEndToDistances startPoints
   in distanceStartEndLoop
  where
    startEndToDistances start =
      let (startToEnd, endNode) =
            walkDesertRec _paths start ((==) 'Z' . last) (cycle _direction)
          -- Account for state of direction cycle after getting to end
          remainingDirections = drop startToEnd $ cycle _direction
          -- Advance end by hand once so the recursion doesn't immediately exit
          afterEnd = advanceLocation _paths (head remainingDirections) endNode
          endToStart =
            fst $
            walkDesertRec
              _paths
              afterEnd
              ((==) endNode)
              (drop 1 $ remainingDirections)
       in (startToEnd, endToStart + 1, start, endNode)

walkDesertMapAtoZ :: DesertMap -> Int
walkDesertMapAtoZ =
  foldl1 lcm . map (\(x, _, _, _) -> x) . findDesertAToZLengths
