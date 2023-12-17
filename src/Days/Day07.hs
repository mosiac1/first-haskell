module Days.Day07
  ( runDay
  ) where

import Data.Char (chr, ord)
import Data.List (sortBy)

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Program.RunDay as R (Day, runDay)
import Util.Util

{- ORMOLU_ENABLE -}
runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: String -> Input
inputParser = map parseCamelPoker . lines

------------ TYPES ------------
type Input = [CamelPoker]

type OutputA = Int

type OutputB = Int

data CamelHand =
  CamelHand (Int, Int, Int, Int, Int)
  deriving (Show)

data CamelPoker = CamelPoker
  { hand :: CamelHand
  , sortedHand :: CamelHand
  , bid :: Int
  } deriving (Show)

------------ PART A ------------
partA :: Input -> OutputA
partA = valuateCamelPokers

------------ PART B ------------
partB :: Input -> OutputB
partB = valuateCamelPokersWithJ . map revalueCamelPokerJ

camelHandToList :: CamelHand -> [Int]
camelHandToList (CamelHand (x, y, z, u, v)) = [x, y, z, u, v]

cardsListToHand :: [Int] -> CamelHand
cardsListToHand [_x, _y, _z, _u, _v] = CamelHand (_x, _y, _z, _u, _v)
cardsListToHand _ = error "Malformed list"

_compareCamelPokers ::
     (CamelHand -> Int) -> CamelPoker -> CamelPoker -> Ordering
_compareCamelPokers ranker (CamelPoker leftHand sortedLeftHand _) (CamelPoker rightHand sortedRightHand _) =
  let leftRanking = ranker sortedLeftHand
      rightRanking = ranker sortedRightHand
   in case leftRanking == rightRanking of
        True ->
          let (leftFirstDif, rightFirstDif) =
                head $
                filter (\(_x, _y) -> _x /= _y) $
                zip (camelHandToList leftHand) (camelHandToList rightHand)
           in compare leftFirstDif rightFirstDif
        False -> compare leftRanking rightRanking

compareCamelPokers :: CamelPoker -> CamelPoker -> Ordering
compareCamelPokers = _compareCamelPokers rankSortedCamelHand

compareCamelPokersWithJ :: CamelPoker -> CamelPoker -> Ordering
compareCamelPokersWithJ = _compareCamelPokers rankSortedCamelHandWithJ

sortCamelHand :: CamelHand -> CamelHand
sortCamelHand _hand =
  let cardsList = camelHandToList _hand
      frequencyMap = Map.fromListWith (+) [(card, 1) | card <- cardsList]
      compareCards lhs rhs =
        let lhsValue = fromJust $ Map.lookup lhs frequencyMap
            rhsValue = fromJust $ Map.lookup rhs frequencyMap
         in case lhsValue == rhsValue of
              True -> compare lhs rhs
              False -> compare lhsValue rhsValue
   in cardsListToHand $ reverse $ sortBy compareCards cardsList

-- Five of a kind - 6
-- Four of a kind - 5
-- Full House - 4
-- Three of a kind - 3
-- Two Pair - 2
-- One Pair - 1
-- High Card - 0
rankSortedCamelHand :: CamelHand -> Int
rankSortedCamelHand (CamelHand (x, y, z, u, v))
  | x == y && x == z && x == u && x == v = 6
  | x == y && x == z && x == u = 5
  | x == y && x == z && u == v = 4
  | x == y && x == z = 3
  | x == y && z == u = 2
  | x == y = 1
  | otherwise = 0

rankSortedCamelHandWithJ :: CamelHand -> Int
rankSortedCamelHandWithJ (CamelHand (x, y, z, u, v))
  | x == y && x == z && x == u && x == v = 6 -- Five of kind, simple
  | x == y && x == z && x == u -- Four of a kind, no Js simple, with any Js -> five
   =
    case countJ of
      0 -> 5
      _ -> 6
  | x == -1 && x == y && x == z && u == v = 6 -- Full house, pair of three is Js "JJJXX"
  | x == y && x == z && u /= -1 && u == v = 4 -- Full house, no Js
  | x /= -1 && x == y && x == z -- Three of a kind, possible Js
   =
    case countJ of
      0 -> 3
      1 -> 5
      _ -> 6
  | x /= -1 && x == y && z /= -1 && z == u -- Two pairs,
   =
    case countJ of
      0 -> 2
      _ -> 4
  | x /= -1 && x == y =
    case countJ of
      0 -> 1
      1 -> 3
      2 -> 5
      _ -> 5
  | otherwise =
    case countJ of
      0 -> 0
      1 -> 1
      2 -> 3
      3 -> 5
      _ -> 6
  where
    countJ =
      length $
      filter (\_x -> _x == -1) $ camelHandToList (CamelHand (x, y, z, u, v))

camelCardValues :: Map.Map Char Int
camelCardValues =
  Map.fromList $
  [('A', 14), ('K', 13), ('Q', 12), ('J', 11), ('T', 10)] ++
  map (\x -> (chr $ ord '0' + x, x)) [1,2 .. 9]

camelCardValuesWithJ :: Map.Map Char Int
camelCardValuesWithJ =
  Map.fromList $
  [('A', 14), ('K', 13), ('Q', 12), ('J', -1), ('T', 10)] ++
  map (\x -> (chr $ ord '0' + x, x)) [1,2 .. 9]

_parseCamelPoker :: Map Char Int -> String -> CamelPoker
_parseCamelPoker cardValues s =
  let (cardsChars, bidChars) = headAndTail $ words s
      _hand =
        cardsListToHand $
        map (\c -> fromJust $ Map.lookup c cardValues) cardsChars
      _bid = parseInt bidChars
   in (CamelPoker _hand (sortCamelHand _hand) _bid)

parseCamelPoker :: String -> CamelPoker
parseCamelPoker = _parseCamelPoker camelCardValues

revalueCamelPokerJ :: CamelPoker -> CamelPoker
revalueCamelPokerJ (CamelPoker _hand _ _bid) =
  let newHand =
        cardsListToHand $
        map
          (\c ->
             if c == 11
               then -1
               else c) $
        camelHandToList _hand
   in (CamelPoker newHand (sortCamelHand newHand) _bid)

_valuateCamelPokers ::
     (CamelPoker -> CamelPoker -> Ordering) -> [CamelPoker] -> Int
_valuateCamelPokers comparator =
  sum . map (\(x, y) -> x * y) . zip [1 ..] . map bid . sortBy comparator

valuateCamelPokers :: [CamelPoker] -> Int
valuateCamelPokers = _valuateCamelPokers compareCamelPokers

valuateCamelPokersWithJ :: [CamelPoker] -> Int
valuateCamelPokersWithJ = _valuateCamelPokers compareCamelPokersWithJ
