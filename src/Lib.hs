module Lib
  ( parseInt
  , headAndTail
  , matchSpel
  , parseDigSpel
  , parseCubeGame
  , sumValidCubeGames
  , sumMinFeasibleCubeGrabPowers
  , parseEngineSchemaLine
  , validNumbersInEngineSchema
  , sumGearRatios
  ) where

import Control.Monad.Random
import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Data.Traversable (mapAccumL, mapAccumR)
import GHC.Utils.Monad
import Text.Regex.Posix

-- Library Function
parseInt :: String -> Int
parseInt = read

headAndTail :: [a] -> (a, a)
headAndTail [] = error "headAndTail: Error no elements"
headAndTail [x] = (x, x)
headAndTail (x:xs) = (x, last xs)

matchSpel :: [Char] -> Maybe Int
matchSpel s =
  if isPrefixOf "one" s
    then Just 1
    else if isPrefixOf "two" s
           then Just 2
           else if isPrefixOf "three" s
                  then Just 3
                  else if isPrefixOf "four" s
                         then Just 4
                         else if isPrefixOf "five" s
                                then Just 5
                                else if isPrefixOf "six" s
                                       then Just 6
                                       else if isPrefixOf "seven" s
                                              then Just 7
                                              else if isPrefixOf "eight" s
                                                     then Just 8
                                                     else if isPrefixOf "nine" s
                                                            then Just 9
                                                            else Nothing

parseDigSpel :: [Char] -> [Maybe Int]
parseDigSpel [] = [Nothing]
parseDigSpel (x:xs) =
  [ if (isDigit x)
      then Just (digitToInt x)
      else matchSpel ([x] ++ xs)
  ] ++
  parseDigSpel xs

-- Day 2 --
data CubeGrab = CubeGrab
  { green :: Int
  , blue :: Int
  , red :: Int
  } deriving (Show)

data CubeGame = CubeGame
  { gameId :: Int
  , grabs :: [CubeGrab]
  } deriving (Show)

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

-- Day 3
data EngineSchemaPoint
  = Dot
  | Symbol Char
  | Number Int Int
  deriving (Show)

isNumber :: EngineSchemaPoint -> Bool
isNumber (Number _ _) = True
isNumber _ = False

getNumber :: EngineSchemaPoint -> Int
getNumber (Number x _) = x
getNumber _ = error "Getting number from non-number EngineSchemaPoint"

getId :: EngineSchemaPoint -> Int
getId (Number _ esId) = esId
getId _ = error "Getting id from non-number EngineSchemaPoint"

prependDigit :: Int -> Int -> Int
prependDigit digit originalNumber = read (show digit ++ show originalNumber)

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
parseEngineSchemaLine :: [Char] -> IO [EngineSchemaPoint]
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
