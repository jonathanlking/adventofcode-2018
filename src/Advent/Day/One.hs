{-# LANGUAGE TemplateHaskell #-}

module Advent.Day.One
  ( solution1
  , solution2
  )  where

import Protolude

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import           Data.FileEmbed (embedFile)

input :: BS.ByteString
input = $(embedFile "resources/day1/input.txt")

numbers :: Maybe [Int]
numbers = sequence $ (map fst . BS.readInt) <$> BS.lines input

solution1 = sum <$> numbers

firstDup :: Ord a => [a] -> Maybe a
firstDup = firstDup' Set.empty
  where
    firstDup' :: Ord a => Set.Set a -> [a] -> Maybe a
    firstDup' _ [] = Nothing
    firstDup' s (x:xs)
      | Set.member x s = Just x
      | otherwise      = firstDup' (Set.insert x s) xs

solution2 = do
  ns <- numbers
  let frequencies = scanl (+) 0 (cycle ns)
  firstDup frequencies
