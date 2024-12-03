module Days.Day17 where

import Data.Array (Array, (!), (//))
import qualified Data.Array as Array
import Data.Char (digitToInt)

import Control.Monad.State
import Data.Attoparsec.Text

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as Q
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Void
import qualified Program.RunDay as R (Day, runDay)
import qualified Util.Util as U

{- ORMOLU_ENABLE -}
runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: String -> Input
inputParser s =
  let sLines = lines s
      height = length sLines
      width = length $ head sLines
      values = map (map digitToInt) sLines
   in Array.array ((1, 1), (height, width)) $
      concat $
      map
        (\(posX, elems) -> map (\(posY, _elem) -> ((posX, posY), _elem)) elems) $
      zip [1 .. height] $ map (zip [1 .. width]) values

------------ TYPES ------------
type Input = CostMatrix

type OutputA = String

type OutputB = Int

type CostMatrix = Array (Int, Int) Int

type VisMatrix = Array (Int, Int) (Int, Int, (Int, Int), (Int, Int))

data Orient
  = Vertical
  | Horizontal
  deriving (Show, Eq)

data DVal = DVal
  { orient :: Orient
  , cost :: Int
  , posX :: Int
  , posY :: Int
  } deriving (Show, Eq)

instance Ord DVal where
  compare (DVal _ lCost _ _) (DVal _ rCost _ _) = lCost `compare` rCost

data Dyno = Dyno
  { q :: MinQueue DVal
  , vis :: VisMatrix
  } deriving (Show, Eq)

------------ PART A ------------
--partA :: Input -> OutputA
partA = dijkstra

------------ PART B ------------
partB :: Input -> OutputB
partB _ = 0

neighbours :: CostMatrix -> Orient -> Int -> Int -> [DVal]
neighbours costs orient x y =
  let (_, (height, width)) = Array.bounds costs
   in case orient
        -- We got in horizontally, going out vertically and vice-versa
            of
        Horizontal ->
          withCostV (reverse [max 1 (x - 3) .. max 1 (x - 1)]) ++
          withCostV [min height (x + 1) .. min height (x + 3)]
        Vertical ->
          withCostH (reverse [max 1 (y - 3) .. max 1 (y - 1)]) ++
          withCostH [min width (y + 1) .. min width (y + 3)]
  where
    withCostV :: [Int] -> [DVal]
    withCostV is =
      snd $
      mapAccumL
        (\acc i ->
           let newAcc = acc + costs ! (i, y)
            in (newAcc, (DVal Vertical newAcc i y)))
        0
        is
    withCostH :: [Int] -> [DVal]
    withCostH js =
      snd $
      mapAccumL
        (\acc j ->
           let newAcc = acc + costs ! (x, j)
            in (newAcc, (DVal Horizontal newAcc x j)))
        0
        js

rec :: CostMatrix -> State Dyno Int
rec costs = do
  (DVal curOrient curCost curPosX curPosY) <- gets $ Q.findMin . q
  case curPosX == h && curPosY == w of
    True -> return curCost
    False -> do
      modify $ (\(Dyno q vis) -> (Dyno (Q.drop 1 q) vis))
      _ <-
        mapM (updateOnNbgh curCost curPosX curPosY) $ neighbours costs curOrient curPosX curPosY
      res <- rec costs
      return res
  where
    (_, (h, w)) = Array.bounds costs
    updateOnNbgh curCost curPosX curPosY (DVal nOr nCost nX nY) = do
      vis <- gets $ vis
      let (visV, visH, prevPosV, prevPosH) = vis ! (nX, nY)
          curVisCost =
            case nOr of
              Vertical -> visV
              Horizontal -> visH
          newVisCost = nCost + curCost
      case curVisCost > newVisCost of
        False -> return ()
        True -> do
          modify $
            (\(Dyno q v) ->
               let newQ = Q.insert (DVal nOr newVisCost nX nY) q
                   newVisEl =
                     case nOr of
                       Vertical -> (newVisCost, visH, (curPosX, curPosY), prevPosH)
                       Horizontal -> (visV, newVisCost, prevPosV, (curPosX, curPosX))
                   newVis = v // [((nX, nY), newVisEl)]
                in (Dyno newQ newVis))

--dijkstra :: CostMatrix -> Int
dijkstra costs =
  let cBounds@(_, (h, w)) = Array.bounds costs
      iniQ =
        Q.insert (DVal Horizontal 0 1 1) $ Q.singleton (DVal Vertical 0 1 1)
      vis =
        Array.array cBounds $
        [((1, 1), (0, 0, (-1, -1), (-1, -1)))] ++
        [ ((i, j), (10 ^ 6, 10 ^ 6, (-1, -1), (-1, -1)))
        | i <- [1 .. h]
        , j <- [1 .. w]
        , i /= 1 || j /= 1
        ]
   in runState (rec costs) (Dyno iniQ vis)
