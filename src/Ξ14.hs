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

part2 :: Set (Int,Int) -> Int
part2 s = length $ concat $ scanl sim [(500,0)] steps
   where
   steps = depth <$> [1..succ $ S.findMax $ S.map snd s]
   depth d = filter (∉ s) $ (,d) . (+ 500) <$> [negate d..d]
   sim a = filter $ \(x,y) -> or [ (x',pred y) ∈ a | x' <- [pred x..succ x] ]

