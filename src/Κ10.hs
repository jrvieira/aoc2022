module Κ10 where

import Zero.Zero

import Data.List ( sort )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/10.txt"
   teqt "part 1" 13140 $ part1 input
   putStr $ part2 input
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/10.txt"
   print $ part1 input
   putStr $ part2 input
   pure ()

parse :: String -> [Op]
parse = map instruction . lines

data Op = Noop | Add Int

instruction :: String -> Op
instruction s
   | ["addx",x] <- words s = Add (read x)
   | "noop" <- s = Noop
   | otherwise = error $ unwords ["illegal instruction:",s]

-- part 1

part1 :: [Op] -> Int
part1 = strength (take 6 [20,60..]) . signal

signal :: [Op] -> [Int]
signal = (1 :) . go 1
   where
   go :: Int -> [Op] -> [Int]
   go _ [] = []
   go x (o:os)
      | Noop <- o = x : go x os
      | Add n <- o = x : x + n : go (x + n) os

strength :: [Int] -> [Int] -> Int
strength is s = sum $ go 1 (sort is) s
   where
   go :: Int -> [Int] -> [Int] -> [Int]
   go _ [] _ = []
   go i (n:ns) (x:xs)
      | n == i = (i * x) : go (succ i) ns xs
      | otherwise = go (succ i) (n:ns) xs
   go _ _ _ = error "signal lost"

-- part 2

part2 :: [Op] -> String
part2 = unlines . map (zipWith (><) [0..]) . fst . packs (replicate 6 40) . signal

(><) :: Int -> Int -> Char
i >< x = if i ∈ [pred x..succ x] then '#' else '.'
