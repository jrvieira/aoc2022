module Θ08 where

import Zero.Zero

import Data.Char ( digitToInt )
import Data.Map.Strict ( Map )
import Data.Map.Strict qualified as Map
import Data.List ( sortBy )
import Data.Function ( on )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/08.txt"
   teqt "part 1" 21 $ part1 input
   teqt "part 2" 8 $ part2 input
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/08.txt"
   print $ part1 input
   print $ part2 input
   pure ()

parse :: String -> Map (Int,Int) Int
parse ls = Map.fromList $ zip coords $ foldr (<>) [] rows
   where
   coords = (,) <$> take w [0..] <*> take h [0..]
   w = length $ head rows
   h = length rows
   rows = map digitToInt <$> lines ls

-- part 1

part1 :: Map (Int,Int) Int -> Int
part1 m = length $ filter visible $ Map.assocs m
   where
   visible :: ((Int,Int),Int) -> Bool
   visible ((x,y),i) = any null [w,e,n,s] -- # show (i, Map.elems <$> [w,e,n,s])
      where
      w = Map.filterWithKey (\(x',y') i' -> y' == y && x' < x && i' >= i) m
      e = Map.filterWithKey (\(x',y') i' -> y' == y && x' > x && i' >= i) m
      n = Map.filterWithKey (\(x',y') i' -> x' == x && y' < y && i' >= i) m
      s = Map.filterWithKey (\(x',y') i' -> x' == x && y' > y && i' >= i) m

-- part 2

part2 :: Map (Int,Int) Int -> Int
part2 m = maximum $ map scenic $ Map.assocs m
   where
   scenic :: ((Int,Int),Int) -> Int
   scenic ((x,y),i) = product $ viewWhile (< i) . map snd <$> [w,e,n,s] -- # show (i, Map.elems <$> [w,e,n,s])
      where
      w = sortBy (on (flip compare) (fst . fst)) $ Map.assocs $ Map.filterWithKey (\(x',y') _ -> y' == y && x' < x) m
      e = sortBy (on       compare  (fst . fst)) $ Map.assocs $ Map.filterWithKey (\(x',y') _ -> y' == y && x' > x) m
      n = sortBy (on (flip compare) (snd . fst)) $ Map.assocs $ Map.filterWithKey (\(x',y') _ -> x' == x && y' < y) m
      s = sortBy (on       compare  (snd . fst)) $ Map.assocs $ Map.filterWithKey (\(x',y') _ -> x' == x && y' > y) m

viewWhile :: (a -> Bool) -> [a] -> Int
viewWhile p = go 0
   where
   go n [] = n
   go n (x:xs)
      | p x = go (succ n) xs
      | otherwise = succ n
