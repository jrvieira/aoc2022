module Ρ17 where

import Zero.Zero
-- import Zero.Echo

import Data.Set
import Data.Set qualified as Set ( null, map )
import Data.Bifunctor ( first, second )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/17.txt"
   teqt "part 1" 3068 $ part1 input
-- echo $ debug input
   teqt "part 2" 1514285714288 $ part2 input
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/17.txt"
   print $ part1 input
   print $ part2 input
   pure ()

parse :: String -> [Move]
parse = fmap f . init
   where
   f '<' = first pred
   f '>' = first succ
   f  c  = error $ unwords [ "parse error in",show c]

-- part 1

part1 :: [Move] -> Int
part1 = τ . sim predicate
   where
   predicate st = ι st >= 2022

data State = State {
   ζ :: State -> Bool , -- stop condition
   ι :: Int , -- rocks fallen
   ν :: Int , -- steps taken
   ρ :: Set Block , -- falling rock
   π :: [Rock] , -- rocks
   σ :: Set Block , -- at rest
   μ :: [Move] , -- jets
   τ :: Int } -- heigth

instance Show State where
   show st = unwords [show $ ι st,show $ ν st,show $ ρ st,show $ length $ σ st,show $ τ st]

type Block = (Int,Int)
type Rock = Set Block
type Move = Block -> Block

flat, plus, kite, cane, cube :: Rock
flat = fromList [ (x,4) | x <- [2..5] ]
plus = fromList [ (3,6), (2,5), (3,5), (4,5), (3,4) ]
kite = fromList [ (4,6), (4,5), (2,4), (3,4), (4,4) ]
cane = fromList [ (2,y) | y <- [4..7] ]
cube = fromList [ (2,5), (3,5), (2,4), (3,4) ]

d :: Move
d = second pred

sim :: (State -> Bool) -> [Move] -> State
sim p jets = go $ State p 0 0 flat (cycle [plus,kite,cane,cube,flat]) (fromList [ (x,0) | x <- [0..6] ]) (cycle jets) 0

go :: State -> State
go st@(State p i n b (r:rs) s (j:js) t)
-- | False  # show st = undefined
   | p st = st
   | Set.null (intersection db s) = go $ State p i (succ n) db (r:rs) s js t
   | otherwise = go $ State p (succ i) (succ n) (Set.map (second (+ h)) r) rs s' js h
   where

   jb = Set.map j b
   db = Set.map d b'

   b'
      | all (∈ [0..6]) (Set.map fst jb) , Set.null (intersection jb s) = jb
      | otherwise = b

   h = maximum $ Set.map snd s'
   s' = b' <> s

go _ = error "something went wrong"

debug :: [Move] -> [(Block, Char)]
debug = fmap (, '#') . toList . σ . sim predicate
   where
   predicate st = ι st >= 2022

-- part 2

part2 :: [Move] -> Int
part2 jets = 0  # show "need to find a cycle. this turns out to be non-trivial"

