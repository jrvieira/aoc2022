module Ι09 where

import Zero.Zero

import Data.List ( nub )

test :: IO ()
test = do
   input_a <- parse <$> readFile "./tests/09a.txt"
   input_b <- parse <$> readFile "./tests/09b.txt"
   teqt "part 1" 13 $ part1 input_a
   teqt "part 2a" 1 $ part2 input_a
   teqt "part 2b" 36 $ part2 input_b
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/09.txt"
   print $ part1 input
   print $ part2 input
   pure ()

parse :: String -> [(Int,Int)]
parse = go (0,0) . map words . lines
   where
   go xy [] = [xy]
   go (x,y) (m:ms)
      | ["U",n] <- m = [ (x,ys) | ys <- [y,pred y..y - read n] ] <> tail (go (x,y - read n) ms)
      | ["D",n] <- m = [ (x,ys) | ys <- [y,succ y..y + read n] ] <> tail (go (x,y + read n) ms)
      | ["L",n] <- m = [ (xs,y) | xs <- [x,pred x..x - read n] ] <> tail (go (x - read n,y) ms)
      | ["R",n] <- m = [ (xs,y) | xs <- [x,succ x..x + read n] ] <> tail (go (x + read n,y) ms)
      | otherwise = error $ unwords ["invalid move:",concat m]

-- part 1

part1 :: [(Int,Int)] -> Int
part1 = length . nub . init . scanl (><) (0,0)

(><) :: (Int,Int) -> (Int,Int) -> (Int,Int)
(x',y') >< (x,y)
   | abs dx > 1 || abs dy > 1 = (signum dx + x',signum dy + y')
   | otherwise = (x',y')
   where
   dx = x - x'
   dy = y - y'

-- part 2

part2 :: [(Int,Int)] -> Int
part2 = length . nub . (!! 9) . iterate (tail . scanl (><) (0,0))
