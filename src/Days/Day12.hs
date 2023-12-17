module Days.Day12
  ( runDay
  ) where

import Control.Monad.State
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Program.RunDay as R (Day, runDay)
import Util.Util

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: String -> Input
inputParser = map parseSpringsAndBrokenLine . lines

------------ TYPES ------------
type Input = [([Spring], [Int])]

type OutputA = Int

type OutputB = Int

data Spring
  = Damaged
  | Working
  | Unknown
  deriving (Show, Eq, Ord)

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . map (uncurry springAssignments)

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map ((uncurry springAssignments) . (uncurry duplicateSprings))

parseSpringsAndBrokenLine :: String -> ([Spring], [Int])
parseSpringsAndBrokenLine s =
  let [springsChars, brokenChars] = words s
      springs =
        map
          (\c ->
             case c of
               '?' -> Unknown
               '#' -> Damaged
               _ -> Working)
          springsChars
      broken = map parseInt $ splitOn "," brokenChars
   in (springs, broken)

duplicateSprings :: [Spring] -> [Int] -> ([Spring], [Int])
duplicateSprings springs broken =
  ( intercalate [Unknown] $ replicate 5 springs
  , foldl1 (++) $ replicate 5 broken)

springAssignmentsRecMem ::
     [Spring] -> [Int] -> State (Map ([Spring], [Int]) Int) Int
springAssignmentsRecMem [] [] = return 1
springAssignmentsRecMem springs [] =
  case all ((/=) Damaged) springs of
    True -> return 1
    False -> return 0
springAssignmentsRecMem [] _ = return 0
springAssignmentsRecMem (spring:springs) (brokenCount:brokenCounts) = do
  mem <- gets $ Map.lookup ((spring : springs), (brokenCount : brokenCounts))
  case mem of
    Just res -> return res
    Nothing -> do
      res <-
        case spring of
          Working ->
            springAssignmentsRecMem springs (brokenCount : brokenCounts)
          Damaged ->
            let nextElems = take (brokenCount - 1) springs
             in case length nextElems == (brokenCount - 1) &&
                     all ((/=) Working) nextElems of
                  True ->
                    case drop (brokenCount - 1) springs of
                      (Damaged:_) -> return 0
                      (_:otherSprings) ->
                        springAssignmentsRecMem otherSprings brokenCounts
                      [] -> springAssignmentsRecMem [] brokenCounts
                  False -> return 0
          Unknown -> do
            resDamaged <-
              (springAssignmentsRecMem
                 (Damaged : springs)
                 (brokenCount : brokenCounts))
            resWorking <-
              (springAssignmentsRecMem
                 (Working : springs)
                 (brokenCount : brokenCounts))
            return $ resDamaged + resWorking
      modify (Map.insert ((spring : springs), (brokenCount : brokenCounts)) res)
      return res

springAssignments :: [Spring] -> [Int] -> Int
springAssignments springs broken =
  fst $ runState (springAssignmentsRecMem springs broken) Map.empty
