module Δ04 where

import Zero.Zero

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/04.txt"
   teqt "part 1" 2 $ part1 input
   teqt "part 2" 4 $ part2 input
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/04.txt"
   print $ part1 input
   print $ part2 input
   pure ()

parse :: String -> [[Int]]
parse = map parseNums . lines

-- part 1

part1 :: [[Int]] -> Int
part1 = length . filter contained

contained :: [Int] -> Bool
contained [a,b,c,d] = and [a <= c,b >= d] || and [a >= c,b <= d]
contained _ = error "illegal assignment"

-- part 2

part2 :: [[Int]] -> Int
part2 = length . filter overlap

overlap :: [Int] -> Bool
overlap [a,b,c,d] = not . null $ [a..b] ∩ [c..d]
overlap _ = error "illegal assignment"
