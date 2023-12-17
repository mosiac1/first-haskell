module Days.Day10
  ( runDay
  ) where

import Data.Array (Array, (!), (//))
import qualified Data.Array as Array

{- ORMOLU_DISABLE -}
import qualified Data.Set as Set

import qualified Program.RunDay as R (Day, runDay)

{- ORMOLU_ENABLE -}
runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: String -> Input
inputParser = parsePipeMaze

------------ TYPES ------------
type Input = PipeMaze

type OutputA = Int

type OutputB = Int

data Orientation
  = North
  | South
  | East
  | West
  deriving (Show, Enum)

data PipeMazeEl
  = VPipe
  | HPipe
  | NEPipe
  | NWPipe
  | SWPipe
  | SEPipe
  | Ground
  | StartPipe
  deriving (Show, Enum, Eq)

data PipeMaze = PipeMaze
  { pipeMaze :: Array (Int, Int) PipeMazeEl
  } deriving (Show)

------------ PART A ------------
partA :: Input -> OutputA
partA = (`div` 2) . length . walkPipeMaze HPipe West

------------ PART B ------------
partB :: Input -> OutputB
partB = pipeMazeInsideCount HPipe West

pipeMazeRedirects :: PipeMazeEl -> Orientation -> Orientation
pipeMazeRedirects HPipe East = East
pipeMazeRedirects HPipe West = West
pipeMazeRedirects VPipe North = North
pipeMazeRedirects VPipe South = South
pipeMazeRedirects NEPipe South = East
pipeMazeRedirects NEPipe West = North
pipeMazeRedirects NWPipe South = West
pipeMazeRedirects NWPipe East = North
pipeMazeRedirects SWPipe North = West
pipeMazeRedirects SWPipe East = South
pipeMazeRedirects SEPipe North = East
pipeMazeRedirects SEPipe West = South
pipeMazeRedirects el orientation =
  error $ "Invalid redirect: " ++ show el ++ " " ++ show orientation

parsePipeMazeEl :: Char -> PipeMazeEl
parsePipeMazeEl '.' = Ground
parsePipeMazeEl '|' = VPipe
parsePipeMazeEl '-' = HPipe
parsePipeMazeEl 'L' = NEPipe
parsePipeMazeEl 'J' = NWPipe
parsePipeMazeEl '7' = SWPipe
parsePipeMazeEl 'F' = SEPipe
parsePipeMazeEl 'S' = StartPipe
parsePipeMazeEl _ = error "Invalid pipe element"

orientationModifier :: Orientation -> (Int, Int)
orientationModifier North = (-1, 0)
orientationModifier South = (1, 0)
orientationModifier East = (0, 1)
orientationModifier West = (0, -1)

moveMazePosByOrientation :: (Int, Int) -> Orientation -> (Int, Int)
moveMazePosByOrientation (cX, cY) orientation =
  let (dX, dY) = orientationModifier orientation
   in (dX + cX, dY + cY)

-- Looks bad, but its just adding indecies to elements to build the 2d array
parsePipeMaze :: String -> PipeMaze
parsePipeMaze s =
  let sLines = lines s
      height = length sLines
      width = length $ head sLines
   in PipeMaze $
      Array.array ((1, 1), (height, width)) $
      concat $
      map
        (\(posX, elems) -> map (\(posY, _elem) -> ((posX, posY), _elem)) elems) $
      zip [1 .. height] $ map (zip [1 .. width] . map parsePipeMazeEl) sLines

advancePipeMazePos ::
     PipeMaze -> Orientation -> (Int, Int) -> ((Int, Int), Orientation)
advancePipeMazePos (PipeMaze _pipeMaze) orientation (cX, cY) =
  let mazeEl = _pipeMaze ! (cX, cY)
      newOrientation = pipeMazeRedirects mazeEl orientation
      newPos = moveMazePosByOrientation (cX, cY) newOrientation
   in (newPos, newOrientation)

walkPipeMazeRec ::
     PipeMaze -> (Int, Int) -> Orientation -> (Int, Int) -> [(Int, Int)]
walkPipeMazeRec _pipeMaze endPos orientation curPos =
  case curPos == endPos of
    True -> [curPos]
    False ->
      let (newPos, newOrientation) =
            advancePipeMazePos _pipeMaze orientation curPos
       in [curPos] ++ walkPipeMazeRec _pipeMaze endPos newOrientation newPos

findPipeMazeStart :: PipeMaze -> (Int, Int)
findPipeMazeStart (PipeMaze _pipeMaze) =
  let (_, (pipesHeight, pipesWidth)) = Array.bounds _pipeMaze
   in head $
      [ (i, j)
      | i <- [1 .. pipesHeight]
      , j <- [1 .. pipesWidth]
      , _pipeMaze ! (i, j) == StartPipe
      ]

walkPipeMaze :: PipeMazeEl -> Orientation -> PipeMaze -> [(Int, Int)]
walkPipeMaze startEl startOrientation _pipeMaze =
  let startPos = findPipeMazeStart _pipeMaze
      newOrientation = pipeMazeRedirects startEl startOrientation
      newPos = moveMazePosByOrientation startPos newOrientation
   in (startPos : walkPipeMazeRec _pipeMaze startPos newOrientation newPos)

data PipeMazeInOutState
  = Inside
  | Outside
  deriving (Show, Eq)

flipInOut :: PipeMazeInOutState -> PipeMazeInOutState
flipInOut Inside = Outside
flipInOut Outside = Inside

data PipeMazeEdgeState
  = OnEdgeFromNorth
  | OnEdgeFromSouth
  | NotOnEdge
  deriving (Show, Eq)

pipeMazeInsideAccForRow ::
     PipeMaze
  -> (Int -> Int -> Bool)
  -> Int
  -> (Int, PipeMazeInOutState, PipeMazeEdgeState)
  -> Int
  -> (Int, PipeMazeInOutState, PipeMazeEdgeState)
pipeMazeInsideAccForRow (PipeMaze _mazeArr) isInWalk posX (insideCount, inOutState, edgeState) posY =
  case isInWalk posX posY of
    False ->
      case _mazeArr ! (posX, posY) of
        _ ->
          ( insideCount +
            if inOutState == Inside
              then 1
              else 0
          , inOutState
          , edgeState)
    True ->
      case _mazeArr ! (posX, posY) of
        Ground ->
          ( insideCount +
            if inOutState == Inside
              then 1
              else 0
          , inOutState
          , edgeState)
        VPipe -> (insideCount, flipInOut inOutState, edgeState)
        NEPipe -> (insideCount, inOutState, OnEdgeFromNorth)
        NWPipe ->
          case edgeState of
            OnEdgeFromNorth -> (insideCount, inOutState, NotOnEdge)
            OnEdgeFromSouth -> (insideCount, flipInOut inOutState, NotOnEdge)
            _ -> error $ "Inconsistent state at " ++ show (posX, posY)
        SEPipe -> (insideCount, inOutState, OnEdgeFromSouth)
        SWPipe ->
          case edgeState of
            OnEdgeFromSouth -> (insideCount, inOutState, NotOnEdge)
            OnEdgeFromNorth -> (insideCount, flipInOut inOutState, NotOnEdge)
            _ -> error $ "Inconsistent state at " ++ show (posX, posY)
        _ -> (insideCount, inOutState, edgeState)

pipeMazeInsideCount :: PipeMazeEl -> Orientation -> PipeMaze -> Int
pipeMazeInsideCount startEl startOrientation _pipeMaze@(PipeMaze _mazeArr) =
  let (_, (pipesHeight, pipesWidth)) = Array.bounds _mazeArr
      (startX, startY) = findPipeMazeStart _pipeMaze
      walk = walkPipeMaze startEl startOrientation _pipeMaze
      walkSet = Set.fromList walk
      updatedPipeMaze = PipeMaze $ _mazeArr // [((startX, startY), startEl)]
      isInWalk posX posY = Set.member (posX, posY) walkSet
      inCountPerRow posX =
        let (inCount, _, _) =
              foldl
                (pipeMazeInsideAccForRow updatedPipeMaze isInWalk posX)
                (0, Outside, NotOnEdge)
                [1 .. pipesWidth]
         in inCount
   in sum $ map inCountPerRow [1 .. pipesHeight]
