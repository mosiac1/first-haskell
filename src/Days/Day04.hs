module Days.Day04
  ( runDay
  ) where

import Data.List (intersect)
import Data.List.Split (splitOn)
import qualified Program.RunDay as R (Day, runDay)
import Text.Regex.Posix

{- ORMOLU_DISABLE -}
import Util.Util

{- ORMOLU_ENABLE -}
runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: String -> Input
inputParser = map parseScratchCard . lines

------------ TYPES ------------
type Input = [ScratchCard]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . map valueScratchCard

------------ PART B ------------
partB :: Input -> OutputB
partB = valScratchCards

data ScratchCard = ScratchCard
  { cardId :: Int
  , winNums :: [Int]
  , chNums :: [Int]
  } deriving (Show)

parseScratchCard :: String -> ScratchCard
parseScratchCard s =
  ScratchCard {cardId = cId, winNums = head wcNs, chNums = last wcNs}
  where
    cId =
      case s =~ "Card *([0-9]+):" :: [[String]] of
        ([_, cardIdS]:_) -> parseInt cardIdS
        _ -> error $ "Card ID not found " ++ s
    numsS = drop 2 $ dropWhile (/= ':') s
    wcNs = map (map parseInt) $ map words $ splitOn " | " numsS

valueScratchCard :: ScratchCard -> Int
valueScratchCard sc =
  case hits of
    0 -> 0
    x -> 2 ^ (x - 1)
  where
    hits = length $ intersect (winNums sc) (chNums sc)

valScratchCards :: [ScratchCard] -> Int
valScratchCards scs = fst $ foldl f (0, []) cValues
  where
    cValues = map (\sc -> length $ intersect (winNums sc) (chNums sc)) scs
    f :: (Int, [Int]) -> Int -> (Int, [Int])
    f (accV, []) v = f (accV, [0]) v
    f (accV, (x:accL)) v = (accV + x + 1, addLists accL $ replicate v (x + 1))
