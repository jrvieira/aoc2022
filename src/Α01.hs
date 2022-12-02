module Α01 where

import Zero.Zero
import Data.List ( sort )
import Data.List.Split ( splitOn )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/01.txt"
   teqt "part 1" 24000 $ part1 input
   teqt "part 2" 45000 $ part2 input
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/01.txt"
   print $ part1 input
   print $ part2 input
   pure ()

parse :: String -> [[Int]]
parse = map (map read . lines) . splitOn "\n\n"

-- part 1

part1 :: [[Int]] -> Int
part1 = maximum . map sum

-- part 2

part2 :: [[Int]] -> Int
part2 = sum . take 3 . reverse . sort . map sum
