module Main
  ( main
  ) where

import Data.Char (digitToInt, isDigit)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust)
import Lib

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

main :: IO ()
main = interact solveDay4P2
