{-# LANGUAGE TypeApplications #-}

module Program.RunDay
  ( runDay
  , Day
  , Verbosity(Quiet, Timings, Verbose)
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad.Except
import Data.Functor
import Data.Time (diffUTCTime, getCurrentTime)
import Program.Color
import System.Console.ANSI
import System.Directory (doesFileExist)
import Text.Pretty.Simple (pPrint)
import Text.Printf

data Verbosity
  = Quiet
  | Timings
  | Verbose
  deriving (Eq, Show, Ord)

type Day = Verbosity -> String -> IO (Maybe Double, Maybe Double)

class DayRunner i p where
  runDay ::
       (Show a, Show b, Show i)
    => p
    -> (i -> a)
    -> (i -> b)
    -> Program.RunDay.Day

instance DayRunner i (String -> i) where
  runDay inputParser partA partB verbosity inputFile = do
    input <- readInput (return . inputParser) inputFile verbosity
    runner input partA partB verbosity

instance DayRunner i (String -> IO i) where
  runDay inputParser partA partB verbosity inputFile = do
    input <- readInput inputParser inputFile verbosity
    runner input partA partB verbosity

readInput ::
     (Show i) => (String -> IO i) -> String -> Verbosity -> IO (Either String i)
readInput inputParser inputFile verbosity = do
  runExceptT $ do
    inputFileExists <- liftIO $ doesFileExist inputFile
    fileContents <-
      if inputFileExists
        then liftIO $ readFile inputFile
        else throwError $
             unwords
               [ "I couldn't read the input!"
               , "I was expecting it to be at"
               , inputFile
               ]
    liftIO $ do
      i <- inputParser fileContents
      when (verbosity == Verbose) $ do
        liftIO $ putStrLn "Parser output:"
        liftIO $ print i
      return i

runner ::
     (Show a, Show b)
  => Either String i
  -> (i -> a)
  -> (i -> b)
  -> Verbosity
  -> IO (Maybe Double, Maybe Double)
runner input partA partB verbosity = do
  case input of
    Left x -> withColor Red (putStrLn x) >> return (Nothing, Nothing)
    Right i -> do
      withColor Blue $ putStrLn "Part A:"
      time1 <- getCurrentTime
      successA <-
        catch (print (partA i) $> True) $ \(m :: SomeException) ->
          withColor Red $ do
            putStrLn "Couldn't run Part A!"
            when (verbosity == Verbose) $ print m
            return False
      time2 <- getCurrentTime
      let timeA = realToFrac $ diffUTCTime time2 time1
      when (verbosity >= Timings && successA) $ putStrLn $ printf "(%.2f)" timeA
      withColor Blue $ putStrLn "Part B:"
      successB <-
        catch (print (partB i) $> True) $ \(m :: SomeException) ->
          withColor Red $ do
            putStrLn "Couldn't run Part B!"
            when (verbosity == Verbose) $ print m
            return False
      time3 <- getCurrentTime
      let timeB = realToFrac $ diffUTCTime time3 time2
      when (verbosity >= Timings && successB) $ putStrLn $ printf "(%.2f)" timeB
      return $
        (,)
          (if successA
             then Just timeA
             else Nothing)
          (if successB
             then Just timeB
             else Nothing)
