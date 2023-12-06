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
  , parseScratchCard
  , valueScratchCard
  , valScratchCards
  , parseEntMap
  , intrnlCombineEntMappings
  , EntMapping(EntMapping)
  , combineEntMap
  , getMappedEnt
  , applyEntMapToSeedRanges
  , mappings
  ) where

import Control.Monad.Random
import Data.Char (digitToInt, isDigit)
import Data.Function (on)
import Data.List (find, intercalate, intersect, isPrefixOf, sortBy)
import Data.List.Split (splitOn)
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
