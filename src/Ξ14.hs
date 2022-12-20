module Ξ14 where

import Zero.Zero

import Data.List.Split ( splitOn )
import Data.Set ( Set )
import Data.Set qualified as S

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/14.txt"
   teqt "part 1" 24 $ part1 input
   teqt "part 2" 93 $ part2 input
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/14.txt"
   print $ part1 input
   print $ part2 input
   pure ()

parse :: String -> Set (Int,Int)
parse = S.fromList  .  concatMap (draw . map (\t -> read $ "(" <> t <> ")") . splitOn " -> ") . lines

draw :: [(Int,Int)] -> [(Int,Int)]
draw ((xa,ya):(xb,yb):r) = [ (x,y) | x <- [min xa xb..max xa xb] , y <- [min ya yb..max ya yb] ] <> draw ((xb,yb):r)
draw r = r

-- part 1

part1 :: Set (Int,Int) -> Int
part1 s = S.size (pour s) - S.size s
   where
   abyss = S.findMax $ S.map snd s
   pour :: Set (Int,Int) -> Set (Int,Int)
   pour s' = go (500,0)
      where
      go (x,y)
      -- | False  # show (abyss,x,y) = undefined
         | y > abyss            = s'
         | (     x,succ y) ∉ s' = go (     x,succ y)
         | (pred x,succ y) ∉ s' = go (pred x,succ y)
         | (succ x,succ y) ∉ s' = go (succ x,succ y)
         | otherwise            = pour $ S.insert (x,y) s'  -- # show (x,y)

-- part 2

-- part2 :: Set (Int,Int) -> Int
-- part2 s = S.size (pour (500,0) s) - S.size s
--    where
--    abyss = S.findMax $ S.map snd s
--    pour :: (Int,Int) -> Set (Int,Int) -> Set (Int,Int)
--    pour o = go o o
--       where
--       go mem (x,y) s'
--       -- | False  # show (abyss,x,y) = undefined
--          | y > abyss              = go o mem $ S.insert (x,y) s'
--          | (     x,succ y) ∉ s'   = go (x,y) (     x,succ y) s'
--          | (pred x,succ y) ∉ s'   = go (x,y) (pred x,succ y) s'
--          | (succ x,succ y) ∉ s'   = go (x,y) (succ x,succ y) s'
--          | (500,0) <- (x,y)       = S.insert (x,y) s'
--          | otherwise              = go o mem $ S.insert (x,y) s'

-- automata

part2 :: Set (Int,Int) -> Int
part2 s = length $ concat $ scanl sim [(500,0)] $ depth <$> [1..succ abyss]
   where
   abyss = S.findMax $ S.map snd s
   depth d = [ (x,d) | x <- (+ 500) <$> [negate d..d] , (x,d) ∉ s ]
   sim a b = filter f b
      where
      f (500,0) = True
      f (x',y') = or [ (x,pred y') ∈ a | x <- [pred x'..succ x'] ]

