module Days.Day03
  ( runDay
  ) where

{- ORMOLU_DISABLE -}
import qualified Data.Set as Set

import Control.Monad.Random.Lazy (randomIO)
import Data.Char (digitToInt, isDigit)
import Data.List (mapAccumR)
import GHC.Utils.Monad (mapAccumLM)
import qualified Program.RunDay as R (Day, runDay)
import Util.Util (prependDigit)

{- ORMOLU_ENABLE -}
runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: String -> IO Input
inputParser = mapM parseEngineSchemaLine . lines

------------ TYPES ------------
type Input = [[EngineSchemaPoint]]

type OutputA = Int

type OutputB = Int

data EngineSchemaPoint
  = Dot
  | Symbol Char
  | Number Int Int
  deriving (Show)

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . validNumbersInEngineSchema

------------ PART B ------------
partB :: Input -> OutputB
partB = sumGearRatios

isNumber :: EngineSchemaPoint -> Bool
isNumber (Number _ _) = True
isNumber _ = False

getNumber :: EngineSchemaPoint -> Int
getNumber (Number x _) = x
getNumber _ = error "Getting number from non-number EngineSchemaPoint"

getId :: EngineSchemaPoint -> Int
getId (Number _ esId) = esId
getId _ = error "Getting id from non-number EngineSchemaPoint"

mapWithLastLM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m [b]
mapWithLastLM f initial =
  fmap snd .
  mapAccumLM
    (\acc x -> do
       res <- f acc x
       return (res, res))
    initial

mapWithLastR :: (b -> a -> b) -> b -> [a] -> [b]
mapWithLastR f initial =
  snd .
  mapAccumR
    (\acc x ->
       let res = f acc x
        in (res, res))
    initial

type Matrix a = [[a]]

foldlMatrixWithNeighbours ::
     (a -> (b, [(b, (Int, Int))]) -> a) -> a -> Matrix b -> a
foldlMatrixWithNeighbours f ini matrix =
  foldl
    f
    ini
    [elementAndNeighbors x y | x <- [0 .. rows - 1], y <- [0 .. cols - 1]]
  where
    rows = length matrix
    cols =
      if null matrix
        then 0
        else length (head matrix)
    --    elementAndNeighbors :: b -> b -> (b, [(b, Int, Int)])
    elementAndNeighbors row col = (cur, zip neighbors neighborPos)
      where
        cur = matrix !! row !! col
        neighborPos =
          [ (r, c)
          | r <- [row - 1 .. row + 1]
          , c <- [col - 1 .. col + 1]
          , r >= 0
          , c >= 0
          , r < rows
          , c < cols
          ]
        neighbors = [matrix !! r !! c | (r, c) <- neighborPos]

-- 467..114.. -> (467)(467)(467)..(114)(114)..
-- Two pass. Pass 1 - fold digits into numbers. Pass 2 -> propagate numbers
parseEngineSchemaLine :: String -> IO [EngineSchemaPoint]
parseEngineSchemaLine =
  fmap
    (mapWithLastR
       (\lastEl x ->
          case x of
            Number cNum cnId ->
              case lastEl of
                Number lNum lnId -> Number lNum lnId
                _ -> Number cNum cnId
            a -> a)
       Dot) .
  mapWithLastLM
    (\lastEl x -> do
       if isDigit x
         then case lastEl of
                Number lNum lnId ->
                  return $ Number (prependDigit lNum $ digitToInt x) lnId
                _ ->
                  (do randId <- randomIO
                      return $ Number (digitToInt x) randId)
         else return $
              case x of
                '.' -> Dot
                _ -> Symbol x)
    Dot

validNumbersInEngineSchema :: Matrix EngineSchemaPoint -> [Int]
validNumbersInEngineSchema =
  map fst . Set.toList . Set.fromList . (foldlMatrixWithNeighbours f [])
  where
    f acc (cur, neighbors) =
      case cur of
        Symbol _ ->
          [(getNumber esp, getId esp) | (esp, _) <- neighbors, isNumber esp] ++
          acc
        _ -> acc

sumGearRatios :: Matrix EngineSchemaPoint -> Int
sumGearRatios = foldlMatrixWithNeighbours f 0
  where
    f acc (cur, neighbors) =
      let numNeighbors =
            map fst $
            Set.toList $
            Set.fromList
              [(getNumber esp, getId esp) | (esp, _) <- neighbors, isNumber esp]
       in case cur of
            Symbol '*' ->
              if length numNeighbors == 2
                then product numNeighbors + acc
                else acc
            _ -> acc
