module Η07 where

import Zero.Zero

import Data.List ( tails, sort )
import Data.Char ( isDigit )
import Data.Map.Lazy ( Map )
import Data.Map qualified as Map

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/07.txt"
   teqt "part 1" 95437 $ part1 input
   teqt "part 2" 24933642 $ part2 input
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/07.txt"
   print $ part1 input
   print $ part2 input
   pure ()

parse :: String -> [String]
parse = lines

-- part 1

part1 :: [String] -> Int
part1 = sum . filter (<= 100000) . Map.elems . dir

type Fs = Map [String] Int

dir :: [String] -> Fs
dir = go [] Map.empty
   where
   go :: [String] -> Fs -> [String] -> Fs
   go _ fs [] = fs
   go cwd fs (c:cs)
      | "$ cd .." <- c = go (tail cwd) fs cs
      | ["$","cd",x] <- words c = go (x:cwd) fs cs
      | "$ ls" <- c = go cwd (Map.insert cwd 0 fs) cs
      | isDigit $ head $ c = go cwd (foldr (.) id (Map.adjust (+ read (head $ words c)) <$> tails cwd) fs) cs
      | otherwise = go cwd fs cs

-- part 2

part2 :: [String] -> Int
part2 cs = minimum $ filter (>= need) $ Map.elems fs
   where
   need = 30000000 - free
   free = 70000000 - fs Map.! ["/"]
   fs = dir cs
