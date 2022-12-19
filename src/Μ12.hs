module Μ12 where

import Zero.Zero

import Data.Maybe ( catMaybes )
import Data.List ( find )
import Data.Set qualified as S ( fromList )
import Data.Map.Strict ( Map )
import Data.Map.Strict qualified as M
import Algorithm.Search

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/12.txt"
   teqt "part 1" 31 $ part1 input
   teqt "part 2" 29 $ part2 input
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/12.txt"
   print $ part1 input
   print $ part2 input
   pure ()

type Pos = (Int,Int)
type Point = (Pos,Int)
type Grid = Map Pos Int

parse :: String -> Grid
parse = grid . fmap (fmap num) . lines
   where
   num c
      | 'S' <- c = pred $ fromEnum 'a'
      | 'E' <- c = succ $ fromEnum 'z'
      | otherwise = fromEnum c
   grid :: [[Int]] -> Grid
   grid = gox (0,0) mempty
      where
      gox _ s [] = s
      gox (x,y) s (l:ls) = gox (x,succ y) (goy (x,y) s l) ls
      goy _ s [] = s
      goy (x,y) s (n:ns) = goy (succ x,y) (M.insert (x,y) n s) ns

-- part 1

part1 :: Map Pos Int -> Int
part1 g = maybe (error "no path") fst $ aStar vertices cost heuristic goal start
   where
   vertices (x,y) = M.keys $ M.filter (<= succ (g M.! (x,y))) $ M.restrictKeys g (S.fromList [(pred x,y),(x,pred y),(succ x,y),(x,succ y)])
   cost _ _ = 1
   heuristic (x,y) = (s-x) + (s-y) -- # show (x,y)
   goal x = (x ==) $ maybe (error "End not found") fst $ find ((== succ (fromEnum 'z')) . snd) $ M.assocs g
   start = maybe (error "Start not found") fst $ find ((== pred (fromEnum 'a')) . snd) $ M.assocs g
   s = pred $ length [ () | (0,_) <- M.keys g ]

-- part 2

part2 :: Map Pos Int -> Int
part2 g = minimum $ map fst $ catMaybes $ map (aStar vertices cost heuristic goal) $ M.keys $ M.filter (== fromEnum 'a') g
   where
   vertices (x,y) = M.keys $ M.filter (<= succ (g M.! (x,y))) $ M.restrictKeys g (S.fromList [(pred x,y),(x,pred y),(succ x,y),(x,succ y)])
   cost _ _ = 1
   heuristic (x,y) = (s-x) + (s-y) -- # show (x,y)
   goal x = (x ==) $ maybe (error "End not found") fst $ find ((== succ (fromEnum 'z')) . snd) $ M.assocs g
   s = pred $ length [ () | (0,_) <- M.keys g ]
