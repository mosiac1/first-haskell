module Days.Day05
  ( runDay
  ) where

import Data.Function (on)
import Data.List (find, intercalate, sortBy, sortOn)
import Data.List.Split (chunksOf, splitOn)
import qualified Program.RunDay as R (Day, runDay)
import Text.Regex.Posix
import Util.Util

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: String -> Input
inputParser s =
  let inLines = splitOn "\n\n" s
      seeds = map parseInt $ words $ drop 7 $ head inLines
      entMap = foldl1 combineEntMap $ map parseEntMap $ drop 1 $ inLines
   in (seeds, entMap)

------------ TYPES ------------
type Input = ([Int], EntMap)

type OutputA = Int

type OutputB = Int

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

------------ PART A ------------
partA :: Input -> OutputA
partA (seeds, entMap) = minimum $ map (getMappedEnt entMap) seeds

------------ PART B ------------
partB :: Input -> OutputB
partB (iniSeeds, entMap) =
  let seeds =
        sortOn (\(EntMapping _ x _) -> x) $
        map (\[x, y] -> (EntMapping x x y)) $ chunksOf 2 iniSeeds
   in minimum $
      map (\(EntMapping _ x _) -> x) $
      applyEntMapToSeedRanges
        seeds
        (sortOn (\(EntMapping x _ _) -> x) $ mappings entMap)

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
