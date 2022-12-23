module Ο15 where

import Zero.Zero
import Zero.Echo

import Control.Arrow ( (&&&) )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/15.txt"
   teqt "part 1" 26 $ part1 10 input
   visualize 20 input
   teqt "part 2" 56000011 $ part2 20 input
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/15.txt"
   print $ part1 2000000 input
   print $ part2 4000000 input
   pure ()

parse :: String -> [((Int,Int),(Int,Int))]
parse = map (pos . parseNums) . lines

pos :: [Int] -> ((Int,Int),(Int,Int))
pos [xa,ya,xb,yb] = ((xa,ya),(xb,yb))
pos e = error $ unwords ["bad input @",show e]

-- part 1

part1 :: Int -> [((Int,Int),(Int,Int))] -> Int
part1 r s = length $ filter vazio row
   where

   row = [ (x,r) | x <- [min_x..max_x] , (x,r) ∉ (snd <$> s) ]
   min_x = minimum $ uncurry (-) . (fst . fst &&& dist) <$> s
   max_x = maximum $ uncurry (+) . (fst . fst &&& dist) <$> s

   vazio :: (Int,Int) -> Bool
   vazio (x,y) = any inside ranges
      where
      inside ((x',y'),d) = d >= dist ((x,y),(x',y'))

   ranges :: [((Int, Int), Int)]
   ranges = (fst &&& dist) <$> s

dist :: ((Int,Int),(Int,Int)) -> Int
dist ((x,y),(x',y')) = abs (x - x') + abs (y - y')

-- part 2

part2 :: Int -> [((Int,Int),(Int,Int))] -> Int
part2 r s = (\(x,y) -> x * 4000000 + y) $ head $ filter (not . vazio) $ {- trace "sus" length $ -} filter local $ concatMap perimeter s
   where

   local :: (Int,Int) -> Bool
   local (x,y) = x >= 0 && x <= r && y >= 0 && y <= r

   vazio :: (Int,Int) -> Bool
   vazio (x,y) = (x,y) ∈ bs || any inside ranges
      where
      inside ((x',y'),d) = d >= dist ((x,y),(x',y'))

   bs :: [(Int,Int)]
   bs = uncurry (∪) $ unzip s

   ranges :: [((Int,Int),Int)]
   ranges = (fst &&& dist) <$> s

perimeter :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
perimeter p@((x,y),_) = zip xs (reverse yα <> tail yα) <> zip xs (yω <> tail (reverse yω))  -- # unwords ["perimeter",show (x,y)]
   where
   xs = [x - d'..x + d']
   yα = [y - d'..y]
   yω = [y..y + d']
   d' = succ $ dist p

-- visualization

visualize :: Int -> [((Int,Int),(Int,Int))] -> IO ()
visualize r s = do
   print $ minimum &&& maximum            $          concatMap perimeter s ∪ bs
   echo $ map (id &&& paint) $          concatMap perimeter s ∪ bs
   print $ minimum &&& maximum            $ range ∩ (concatMap perimeter s ∪ bs)
   echo $ map (id &&& paint) $ range ∩ (concatMap perimeter s ∪ bs)
   print $ minimum &&& maximum            $ range
   echo $ map (id &&& paint) $ range
   where

   bs :: [(Int,Int)]
   bs = uncurry (∪) $ unzip s

   paint :: (Int, Int) -> Char
   paint c
      | c ∈ (fst <$> s) = 'S'
      | c ∈ (snd <$> s) = 'B'
      | vazio c = '#'
      | otherwise = '.'

   vazio :: (Int,Int) -> Bool
   vazio (x,y) = any inside ranges
      where
      inside ((x',y'),d) = d >= dist ((x,y),(x',y'))

   ranges :: [((Int,Int),Int)]
   ranges = (fst &&& dist) <$> s

   range :: [(Int,Int)]
   range = (,) <$> [0..r] <*> [0..r]

