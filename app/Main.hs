module Main
  ( main
  ) where

import Data.Char (digitToInt, isDigit)
import Data.List (sortBy, sortOn)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (fromJust, isJust)
import Lib
import Text.Pretty.Simple (pPrint)

interactM :: (String -> IO String) -> IO ()
interactM f = do
  input <- getContents
  output <- f input
  putStrLn output

-- Day 1, Part 2 Solution
solveDay1P2 :: [Char] -> String
solveDay1P2 =
  show .
  sum .
  (map $
   (\(x, y) -> fromJust x * 10 + fromJust y) .
   headAndTail . filter isJust . parseDigSpel) .
  (splitOn "\n")

-- Day 1, Part 1 Solution
solveDay1P1 :: [Char] -> String
solveDay1P1 =
  show .
  sum .
  (map $ (\(x, y) -> digitToInt x * 10 + digitToInt y) . firstLastDigits) .
  (splitOn "\n")
  where
    firstLastDigits :: [Char] -> (Char, Char)
    firstLastDigits = headAndTail . filter isDigit

-- Day 2, Part 1 Solution
solveDay2P1 :: [Char] -> String
solveDay2P1 = show . sumValidCubeGames . map parseCubeGame . lines

solveDay2P2 :: [Char] -> String
solveDay2P2 = show . sumMinFeasibleCubeGrabPowers . map parseCubeGame . lines

-- Day 3
solveDay3P1 :: String -> IO String
solveDay3P1 s = do
  engineSchemas <- mapM parseEngineSchemaLine (lines s)
  return $ show $ sum $ validNumbersInEngineSchema engineSchemas

solveDay3P2 :: String -> IO String
solveDay3P2 s = do
  engineSchemas <- mapM parseEngineSchemaLine (lines s)
  return $ show $ sumGearRatios engineSchemas

-- Day 4
solveDay4P1 :: String -> String
solveDay4P1 = show . sum . map (valueScratchCard . parseScratchCard) . lines

solveDay4P2 :: String -> String
solveDay4P2 = show . valScratchCards . map parseScratchCard . lines

-- Day 5
solveDay5P1 :: IO ()
solveDay5P1 = do
  input <- getContents
  let inLines = splitOn "\n\n" input
      seeds = map parseInt $ words $ drop 7 $ head inLines
      entMap = foldl1 combineEntMap $ map parseEntMap $ drop 1 $ inLines
  pPrint $ minimum $ map (getMappedEnt entMap) seeds

solveDay5P2 :: IO ()
solveDay5P2 = do
  input <- getContents
  let inLines = splitOn "\n\n" input
      seeds =
        sortOn (\(EntMapping _ x _) -> x) $
        map (\[x, y] -> (EntMapping x x y)) $
        chunksOf 2 $ map parseInt $ words $ drop 7 $ head inLines
      entMap = foldl1 combineEntMap $ map parseEntMap $ drop 1 $ inLines
  pPrint $
    sortOn (\(EntMapping _ x _) -> x) $
    applyEntMapToSeedRanges
      seeds
      (sortOn (\(EntMapping x _ _) -> x) $ mappings entMap)

-- Day 6
solveDay6P1 :: String -> String
solveDay6P1 =
  show .
  product . map ((\(l, r) -> r - l + 1) . getRaceWinInterv) . parseRaceTimeDist

solveDay6P2 :: String -> String
solveDay6P2 =
  show . (\(l, r) -> r - l + 1) . getRaceWinInterv . parseRaceTimeDistComb

-- Day 7
solveDay7P1 :: String -> String
solveDay7P1 = show . valuateCamelPokers . map parseCamelPoker . lines

solveDay7P2 :: String -> String
solveDay7P2 = show . valuateCamelPokersWithJ . map parseCamelPokerWithJ . lines

-- Day 8
solveDay8P1 :: String -> String
solveDay8P1 = show . walkDesertMap . parseDesertMap

solveDay8P2 :: String -> String
solveDay8P2 = show . walkDesertMapAtoZ . parseDesertMap

-- Day 9
solveDay9P1 :: String -> String
solveDay9P1 = show . sum . map oasisPredict . map reverse . parseOases

solveDay9P2 :: String -> String
solveDay9P2 = show . sum . map oasisPredict . parseOases

-- Day 10
solveDay10P1 :: String -> String
solveDay10P1 =
  show . (`div` 2) . length . walkPipeMaze HPipe West . parsePipeMaze

solveDay10P2 :: String -> String
solveDay10P2 = show . pipeMazeInsideCount HPipe West . parsePipeMaze

-- Day 11
solveDay11P1 = show . galaxySumDistances 1 . parseGalaxyMap

solveDay11P2 = show . galaxySumDistances 999999 . parseGalaxyMap

main :: IO ()
main = interact solveDay11P2
