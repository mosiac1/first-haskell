module Util.Util where

import Data.List (tails)
import Data.Maybe (fromMaybe)

{- ORMOLU_DISABLE -}
import Data.Set (Set)
import qualified Data.Set as Set

{- ORMOLU_ENABLE -}
{-
This module contains a series of miscellaneous utility functions that I have found helpful in the past.
-}
parseInt :: String -> Int
parseInt = read

headAndTail :: [a] -> (a, a)
headAndTail [] = error "headAndTail: Error no elements"
headAndTail [x] = (x, x)
headAndTail (x:xs) = (x, last xs)

addLists :: Num a => [a] -> [a] -> [a]
addLists xs ys =
  let maxLength = max (length xs) (length ys)
      padList list = list ++ replicate (maxLength - length list) 0
      paddedXs = padList xs
      paddedYs = padList ys
   in zipWith (+) paddedXs paddedYs

setCountInRange :: (Ord a) => Set a -> a -> a -> Int
setCountInRange set lower higher =
  fromMaybe 0 $ do
    lowerGE <- Set.lookupGE lower set
    higherLE <- Set.lookupLE higher set
    return $ Set.findIndex higherLE set - Set.findIndex lowerGE set + 1

allPairs :: [a] -> [(a, a)]
allPairs xs = [(x, y) | (x:rest) <- tails xs, y <- rest]

prependDigit :: Int -> Int -> Int
prependDigit digit originalNumber = read (show digit ++ show originalNumber)
