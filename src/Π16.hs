module Π16 where

import Zero.Zero

import Data.List ( intersect, nub )
import Data.List.Split ( splitOn )
import Data.Map.Strict ( Map, (!), empty, fromList, insertWith, keys )
import Control.Arrow ( (&&&) )
import Control.Monad ( join )
import Data.Bifunctor ( bimap )
import Combinatorics ( partitions )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/16.txt"
   teqt "part 1" 1651 $ part1 input
   teqt "part 2" 1707 $ part2 input
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/16.txt"
   print $ part1 input
   print $ part2 input
   pure ()

parse :: String -> [Node]
parse = map (go . words) . lines
   where
   go (_:val:_:_:p:_:_:_:_:vals) = (val,head $ parseNums p,splitOn "," $ concat vals)
   go _ = error "no parse"

type Node = (String,Int,[String])

-- part 1

part1 :: [Node] -> Int
part1 nodes = run pvalves 30 0 "AA"
   where

   -- nonzero nodes (positive valves)
   pvalves :: [(String,Int)]
   pvalves = map (\(n,p,_) -> (n,p)) $ filter (\(_,p,_) -> p > 0) nodes

   tunnels :: Map String [String]
   tunnels = fromList $ (\(n,_,ns) -> (n,ns)) <$> nodes

   -- min distances between pvalves
   distances :: Map String (Map String Int)
   distances = fromList $ (id &&& δ) . fst <$> pvalves

   δ :: String -> Map String Int
   δ v = go 1 (tunnels ! v) empty
      where
      go :: Int -> [String] -> Map String Int -> Map String Int
      go d ns acc
         | length acc == length vs = acc
         | otherwise = go (succ d) ns' acc'
         where
         ns' = filter (∉ keys acc) $ nub $ concatMap (tunnels !) ns
         acc' = foldr (flip (insertWith (const id)) d) acc (intersect vs ns)
      vs = "AA" : (fst <$> remove v pvalves)  -- add origin

   distance :: String -> String -> Int
   distance a b = distances ! b ! a

   -- vs <- remaining valves
   -- t <- time remaining (min)
   -- r <- released pressure
   -- a <- anterior valve
   -- v <- current valve
   -- p <- current valve pressure p/min

   run :: [(String,Int)] -> Int -> Int -> String -> Int
   run vs t r a = maximum $ go <$> vs
      where
      go :: (String,Int) -> Int
      go (v,p)
         | t' <= 0 = r
         | null vs' = r'
         | otherwise = run vs' t' r' v
         where
         r' = r + p * t'
         t' = pred t - distance a v
         vs' = remove v vs

-- part 2

part2 :: [Node] -> Int
part2 nodes = maximum $ uncurry (+) . join bimap (\x -> run x 26 0 "AA") <$> take (div (length ps) 2) ps
   where

   ps :: [([(String,Int)],[(String,Int)])]
   ps = partitions pvalves

   -- nonzero nodes (positive valves)
   pvalves :: [(String,Int)]
   pvalves = map (\(n,p,_) -> (n,p)) $ filter (\(_,p,_) -> p > 0) nodes

   tunnels :: Map String [String]
   tunnels = fromList $ (\(n,_,ns) -> (n,ns)) <$> nodes

   -- min distances between pvalves
   distances :: Map String (Map String Int)
   distances = fromList $ (id &&& δ) . fst <$> pvalves

   δ :: String -> Map String Int
   δ v = go 1 (tunnels ! v) empty
      where
      go :: Int -> [String] -> Map String Int -> Map String Int
      go d ns acc
         | length acc == length vs = acc
         | otherwise = go (succ d) ns' acc'
         where
         ns' = filter (∉ keys acc) $ nub $ concatMap (tunnels !) ns
         acc' = foldr (flip (insertWith (const id)) d) acc (intersect vs ns)
      vs = "AA" : (fst <$> remove v pvalves)  -- add origin

   distance :: String -> String -> Int
   distance a b = distances ! b ! a

   -- vs <- remaining valves
   -- t <- time remaining (min)
   -- r <- released pressure
   -- a <- anterior valve
   -- v <- current valve
   -- p <- current valve pressure p/min

   run :: [(String,Int)] -> Int -> Int -> String -> Int
   run [] _ r _ = r
   run vs t r a = maximum $ go <$> vs
      where
      go :: (String,Int) -> Int
      go (v,p)
         | t' <= 0 = r
         | null vs' = r'
         | otherwise = run vs' t' r' v
         where
         r' = r + p * t'
         t' = pred t - distance a v
         vs' = remove v vs
