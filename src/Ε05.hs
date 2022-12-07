module Ε05 where

import Zero.Zero

import Data.Char ( isLetter )
import Data.List.Split ( chunksOf )
import Data.Bifunctor ( bimap )
import Data.Maybe ( listToMaybe )
import Data.IntMap.Lazy ( IntMap )
import Data.IntMap.Lazy qualified as IntMap

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/05.txt"
   teqt "part 1" "CMZ" $ part1 input
   teqt "part 2" "MCD" $ part2 input
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/05.txt"
   print $ part1 input
   print $ part2 input
   pure ()

parse :: String -> (IntMap [Char],[[Int]])
parse = bimap (stack IntMap.empty . init) (map parseNums . tail) . span (not . null) . lines

stack :: IntMap [Char] -> [String] -> IntMap [Char]
stack m [] = IntMap.map reverse m
stack m (l:ln) = stack m' ln
   where
   m' :: IntMap [Char]
   m' = foldr f m $ zip [1..] $ map head $ chunksOf 4 $ tail l
   f :: (Int,Char) -> IntMap [Char] -> IntMap [Char]
   f (k,c)
      | ' ' <- c = id
      | isLetter c = IntMap.insertWith (<>) k [c]
      | otherwise = error "invalid crate"

-- part 1

part1 :: (IntMap [Char],[[Int]]) -> [Char]
part1 = IntMap.elems . IntMap.mapMaybe listToMaybe . uncurry rearrange
   where
   rearrange m p
      | null p = m
      | ([n,f,t]:ps) <- p = rearrange (IntMap.insertWith (<>) t (reverse $ take n $ m IntMap.! f) . IntMap.adjust (drop n) f $ m) ps
      | otherwise = error "invalid move"

-- part 2

part2 :: (IntMap [Char],[[Int]]) -> [Char]
part2 = IntMap.elems . IntMap.mapMaybe listToMaybe . uncurry rearrange
   where
   rearrange m p
      | null p = m
      | ([n,f,t]:ps) <- p = rearrange (IntMap.insertWith (<>) t (take n $ m IntMap.! f) . IntMap.adjust (drop n) f $ m) ps
      | otherwise = error "invalid move"
