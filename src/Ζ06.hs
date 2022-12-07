module Ζ06 where

import Zero.Zero

import Data.List ( nub )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/06.txt"
   teqt "part 1" [7,5,6,10,11] $ part1 input
   teqt "part 2" [19,23,23,29,26] $ part2 input
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/06.txt"
   print $ part1 input
   print $ part2 input
   pure ()

parse :: String -> [String]
parse = lines

-- part 1

part1 :: [[Char]] -> [Int]
part1 = map (mark 4)

mark :: Int -> [Char] -> Int
mark n = go n
   where
   go i s
      | b <- take n s , nub b == b = i
      | otherwise = go (succ i) (tail s)

-- part 2

part2 :: [[Char]] -> [Int]
part2 = map (mark 14)
