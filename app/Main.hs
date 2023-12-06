module Main
  ( main
  ) where

import Data.Char (digitToInt, isDigit)
import Data.List (sortOn)
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

main :: IO ()
main = solveDay5P2
