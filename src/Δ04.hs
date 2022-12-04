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

parse :: String -> [Assignment]
parse = map (assignment . parseNums) . lines
   where
   assignment l
      | [a,b,c,d] <- l = (a,b,c,d)
      | otherwise = error "illegal assignment"

type Assignment = (Int,Int,Int,Int)

-- part 1

part1 :: [Assignment] -> Int
part1 = length . filter contained

contained :: Assignment -> Bool
contained (a,b,c,d) = a <= c && b >= d || a >= c && b <= d

-- part 2

part2 :: [Assignment] -> Int
part2 = length . filter overlap

overlap :: Assignment -> Bool
overlap (a,b,c,d) = not . null $ [a..b] ∩ [c..d]
