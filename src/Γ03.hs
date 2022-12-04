module Γ03 where

import Zero.Zero

import Data.List.Split ( chunksOf )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/03.txt"
   teqt "part 1" 157 $ part1 input
   teqt "part 2" 70 $ part2 input
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/03.txt"
   print $ part1 input
   print $ part2 input
   pure ()

parse :: String -> [Rucksack]
parse = lines

type Rucksack = [Char]

-- part 1

part1 :: [Rucksack] -> Int
part1 = sum . map (priority . head . uncurry (∩) . rucksack)

rucksack l = splitAt (div (length l) 2) l

priority :: Char -> Int
priority c = succ $ length $ takeWhile (/= c) $ ['a'..'z'] <> ['A'..'Z']

-- part 2

part2 :: [Rucksack] -> Int
part2 = sum . map (priority . head . foldr1 (∩)) . chunksOf 3
