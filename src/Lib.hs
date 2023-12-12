module Lib where

import Control.Monad.Random
import qualified Data.Array as Array
import Data.Array ((!), (//))
import Data.Char (digitToInt, isDigit)
import Data.Char (chr, ord)
import Data.Function (on)
import Data.List (find, intercalate, intersect, isPrefixOf, sortBy, tails)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as Set
import Data.Traversable (mapAccumR)
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

addLists :: Num a => [a] -> [a] -> [a]
addLists xs ys =
  let maxLength = max (length xs) (length ys)
      padList list = list ++ replicate (maxLength - length list) 0
      paddedXs = padList xs
      paddedYs = padList ys
   in zipWith (+) paddedXs paddedYs

setCountInRange :: (Ord a) => Set.Set a -> a -> a -> Int
setCountInRange set lower higher =
  fromMaybe 0 $ do
    lowerGE <- Set.lookupGE lower set
    higherLE <- Set.lookupLE higher set
    return $ Set.findIndex higherLE set - Set.findIndex lowerGE set + 1

allPairs :: [a] -> [(a, a)]
allPairs xs = [(x, y) | (x:rest) <- tails xs, y <- rest]

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

-- Day 4
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

-- Day 5
data EntMapping = EntMapping
  { sourceStart :: Int
  , destStart :: Int
  , size :: Int
  }

data EntMap = EntMap
  { sourceType :: String
  , destType :: String
  , mappings :: [EntMapping]
  }

instance Show EntMapping where
  show (EntMapping sStart dStart sz) =
    show sStart ++ " " ++ show dStart ++ " " ++ show sz

instance Show EntMap where
  show (EntMap sType dType ms) =
    sType ++ "-to-" ++ dType ++ ":\n" ++ (intercalate "\n" $ map show ms)

parseEntMap :: String -> EntMap
parseEntMap s =
  case lines s of
    (header:entries) ->
      let (soType, deType) =
            case header =~ "(.*)-to-(.*) map:" :: [[String]] of
              ([_, _soType, _deType]:_) -> (_soType, _deType)
              _ -> error $ "Error unpacking header - " ++ s
          unpckMapping (soS:deS:sz:_) = (EntMapping deS soS sz)
          unpckMapping _ = error "Error unpacking"
          mps =
            sortBy (compare `on` sourceStart) $
            map (unpckMapping . map parseInt . words) entries
       in (EntMap soType deType mps)
    _ -> error $ "Error unpacking line - " ++ s

-- Assume the RHS is sorted by sourceStart. The LHS needs to be sorted by destStart
combineEntMapping :: [EntMapping] -> [EntMapping] -> [EntMapping]
combineEntMapping lls rls =
  intrnlCombineEntMappings (sortBy (compare `on` destStart) lls) rls

intrnlCombineEntMappings :: [EntMapping] -> [EntMapping] -> [EntMapping]
intrnlCombineEntMappings lls [] = lls
intrnlCombineEntMappings [] rls = rls
intrnlCombineEntMappings ((EntMapping lss lds lsize):ls) ((EntMapping rss rds rsize):rs)
  | lsize == 0 = intrnlCombineEntMappings ls ((EntMapping rss rds rsize) : rs)
  | rsize == 0 = intrnlCombineEntMappings ((EntMapping lss lds lsize) : ls) rs
  | lds + lsize <= rss -- Left before right -> pass through
   =
    ((EntMapping lss lds lsize) :
     intrnlCombineEntMappings ls ((EntMapping rss rds rsize) : rs))
  | rss + rsize <= lds -- Right before left -> pass through
   =
    ((EntMapping rss rds rsize) :
     intrnlCombineEntMappings ((EntMapping lss lds lsize) : ls) rs)
  | lds < rss && lds + lsize >= rss && lds + lsize <= rss + rsize -- Left before right, intersecting but disjunctive
   =
    let leftNoRight = EntMapping lss lds (rss - lds)
        rafSize = lsize - (rss - lds)
        rightAndLeft = EntMapping (lss + lsize - rafSize) rds rafSize
     in (leftNoRight :
         rightAndLeft :
         intrnlCombineEntMappings
           ls
           ((EntMapping
               (rss + rafSize - 1)
               (rds + rafSize - 1)
               (rsize - rafSize)) :
            rs))
  | lds >= rss && lds + lsize <= rss + rsize -- Left included in right
   =
    let lOffset = lds - rss
        leftInRight = EntMapping lss (rds + lOffset) lsize
        rOffset = lOffset + lsize
     in (leftInRight :
         intrnlCombineEntMappings
           ls
           ((EntMapping (rss + rOffset) (rds + rOffset) (rsize - rOffset)) : rs))
  | lds > rss && lds + lsize > rss + rsize -- Left intersecting right, ending after, disjunctive
   =
    let larSize = rss + rsize - lds + 1
        rightNoLeft = EntMapping rss rds (rsize - larSize)
        leftAndRight = EntMapping lss (rds + rsize - larSize) larSize
     in (rightNoLeft :
         leftAndRight :
         intrnlCombineEntMappings
           ((EntMapping (lss + larSize) (lds + larSize) (lsize - larSize)) : ls)
           rs)
  | otherwise -- Left includes right and other elements; just breakup left and recursive call
   =
    let lDiff = rss - lds
        lBeforeR = EntMapping lss lds lDiff
        lAndR = EntMapping (lss + lDiff) (lds + lDiff) rsize
        lAfterR =
          EntMapping
            (lss + lDiff + rsize)
            (lds + lDiff + rsize)
            (lsize - rsize - lDiff)
     in intrnlCombineEntMappings
          (lBeforeR : lAndR : lAfterR : ls)
          ((EntMapping rss rds rsize) : rs)

combineEntMap :: EntMap -> EntMap -> EntMap
combineEntMap (EntMap lhSt _ lhMs) (EntMap _ rhDs rhMs) =
  EntMap lhSt rhDs (combineEntMapping lhMs rhMs)

getMappedEnt :: EntMap -> Int -> Int
getMappedEnt (EntMap _ _ ms) x =
  case find
         (\(EntMapping mStart _ mSize) -> x >= mStart && x < mStart + mSize)
         ms of
    Just (EntMapping resStart resDest _) -> resDest + x - resStart
    Nothing -> x

-- Mostly the same as intrnlCombineEntMappings, but does not return mapping coming purely from the Right-Hand-Side
applyEntMapToSeedRanges :: [EntMapping] -> [EntMapping] -> [EntMapping]
applyEntMapToSeedRanges lls [] = lls
applyEntMapToSeedRanges [] _ = []
applyEntMapToSeedRanges ((EntMapping lss lds lsize):ls) ((EntMapping rss rds rsize):rs)
  | lsize == 0 = applyEntMapToSeedRanges ls ((EntMapping rss rds rsize) : rs)
  | rsize == 0 = applyEntMapToSeedRanges ((EntMapping lss lds lsize) : ls) rs
  | lds + lsize <= rss -- Left before right -> pass through
   =
    ((EntMapping lss lds lsize) :
     applyEntMapToSeedRanges ls ((EntMapping rss rds rsize) : rs))
  | rss + rsize <= lds -- Right before left -> pass through
   = applyEntMapToSeedRanges ((EntMapping lss lds lsize) : ls) rs
  | lds < rss && lds + lsize >= rss && lds + lsize <= rss + rsize -- Left before right, intersecting but disjunctive
   =
    let leftNoRight = EntMapping lss lds (rss - lds)
        rafSize = lsize - (rss - lds)
        rightAndLeft = EntMapping (lss + lsize - rafSize) rds rafSize
     in (leftNoRight :
         rightAndLeft :
         applyEntMapToSeedRanges
           ls
           ((EntMapping
               (rss + rafSize - 1)
               (rds + rafSize - 1)
               (rsize - rafSize)) :
            rs))
  | lds >= rss && lds + lsize <= rss + rsize -- Left included in right
   =
    let lOffset = lds - rss
        leftInRight = EntMapping lss (rds + lOffset) lsize
        rOffset = lOffset + lsize
     in (leftInRight :
         applyEntMapToSeedRanges
           ls
           ((EntMapping (rss + rOffset) (rds + rOffset) (rsize - rOffset)) : rs))
  | lds > rss && lds + lsize > rss + rsize -- Left intersecting right, ending after, disjunctive
   =
    let larSize = rss + rsize - lds + 1
        leftAndRight = EntMapping lss (rds + rsize - larSize) larSize
     in (leftAndRight :
         applyEntMapToSeedRanges
           ((EntMapping (lss + larSize) (lds + larSize) (lsize - larSize)) : ls)
           rs)
  | otherwise -- Left includes right and other elements; just breakup left and recursive call
   =
    let lDiff = rss - lds
        lBeforeR = EntMapping lss lds lDiff
        lAndR = EntMapping (lss + lDiff) (lds + lDiff) rsize
        lAfterR =
          EntMapping
            (lss + lDiff + rsize)
            (lds + lDiff + rsize)
            (lsize - rsize - lDiff)
     in applyEntMapToSeedRanges
          (lBeforeR : lAndR : lAfterR : ls)
          ((EntMapping rss rds rsize) : rs)

-- Day 6
parseRaceTimeDist :: String -> [(Int, Int)]
parseRaceTimeDist =
  (\(times, dists) -> zip times dists) .
  headAndTail . map (map parseInt . words . drop 11) . lines

parseRaceTimeDistComb :: String -> (Int, Int)
parseRaceTimeDistComb =
  headAndTail .
  map (foldl prependDigit 0 . map parseInt . words . drop 11) . lines

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

-- Day 7
data CamelHand =
  CamelHand (Int, Int, Int, Int, Int)
  deriving (Show)

data CamelPoker = CamelPoker
  { hand :: CamelHand
  , sortedHand :: CamelHand
  , bid :: Int
  } deriving (Show)

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

_parseCamelPoker :: Map.Map Char Int -> String -> CamelPoker
_parseCamelPoker cardValues s =
  let (cardsChars, bidChars) = headAndTail $ words s
      _hand =
        cardsListToHand $
        map (\c -> fromJust $ Map.lookup c cardValues) cardsChars
      _bid = parseInt bidChars
   in (CamelPoker _hand (sortCamelHand _hand) _bid)

parseCamelPoker :: String -> CamelPoker
parseCamelPoker = _parseCamelPoker camelCardValues

parseCamelPokerWithJ :: String -> CamelPoker
parseCamelPokerWithJ = _parseCamelPoker camelCardValuesWithJ

_valuateCamelPokers ::
     (CamelPoker -> CamelPoker -> Ordering) -> [CamelPoker] -> Int
_valuateCamelPokers comparator =
  sum . map (\(x, y) -> x * y) . zip [1 ..] . map bid . sortBy comparator

valuateCamelPokers :: [CamelPoker] -> Int
valuateCamelPokers = _valuateCamelPokers compareCamelPokers

valuateCamelPokersWithJ :: [CamelPoker] -> Int
valuateCamelPokersWithJ = _valuateCamelPokers compareCamelPokersWithJ

-- Day 8
data DesertDirection
  = DesertLeft
  | DesertRight

data DesertMap = DesertMap
  { direction :: [DesertDirection]
  , paths :: Map.Map String (String, String)
  }

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
     Map.Map String (String, String) -> DesertDirection -> String -> String
advanceLocation desertPaths curDirection curLocation =
  let pathChoices = fromJust $ Map.lookup curLocation desertPaths
   in case curDirection of
        DesertLeft -> fst pathChoices
        DesertRight -> snd pathChoices

walkDesertRec ::
     Map.Map String (String, String)
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

-- Day 9
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

-- Day 10
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
  { pipeMaze :: Array.Array (Int, Int) PipeMazeEl
  } deriving (Show)

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

-- Day 11
data GalaxyMapEl
  = Galaxy
  | Space
  deriving (Show, Eq)

parseGalaxyMapEl :: Char -> GalaxyMapEl
parseGalaxyMapEl s =
  case s of
    '.' -> Space
    '#' -> Galaxy
    c -> error $ "Invalid GalaxyMapEl " ++ show c

data GalaxyMap = GalaxyMap
  { galaxyMap :: Array.Array (Int, Int) GalaxyMapEl
  }

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

--galaxySumDistances :: GalaxyMap -> Int
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
