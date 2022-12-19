module Λ11 where

import Zero.Zero

import Data.List ( foldl', sortBy )
import Data.List.Split ( splitOn )
import Data.IntMap ( IntMap )
import Data.IntMap qualified as I

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/11.txt"
   teqt "part 1" 10605 $ part1 input
   teqt "part 2" 2713310158 $ part2 input
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/11.txt"
   print $ part1 input
   print $ part2 input
   pure ()

parse :: String -> IntMap Monkey
parse = I.fromList . map monkey . splitOn "\n\n"

data Monkey = Monkey {
   inspections :: Int ,
   items :: [Int] ,
   operation :: (Item,Int -> Int -> Int,Item) ,
   mt :: Test }

data Item = Old | Item Int
data Test = Test { d :: Int , t :: Int , f :: Int }
   deriving ( Show )

instance Show Monkey where
   show (Monkey _ its _ tst) = unwords ["M",show its,show tst]

monkey :: String -> (Int,Monkey)
monkey = go . lines
   where
   go [i,its,op,td,tt,tf] = (head (parseNums i) ,) $ Monkey 0 (parseNums its) (itemL op,pop op,itemR op) (Test (head $ parseNums td) (head $ parseNums tt) (head $ parseNums tf))
   go _ = error "?"

itemL :: String -> Item
itemL = go . head . drop 3 . words
   where
   go "old" = Old
   go x = Item $ read x

itemR :: String -> Item
itemR = go . head . drop 5 . words
   where
   go "old" = Old
   go x = Item $ read x

pop :: String -> Int -> Int -> Int
pop s
   | '*' ∈ s = (*)
   | '+' ∈ s = (+)
   | otherwise = error "?"

-- part 1

part1 :: IntMap Monkey -> Int
part1 = mbusiness . (!! 20) . iterate mround

mbusiness :: IntMap Monkey -> Int
mbusiness = product . take 2 . sortBy (flip compare) . map inspections . I.elems

mround :: IntMap Monkey -> IntMap Monkey
mround = go 0
   where
   go i ms
      | Just m  <- ms I.!? i = go (succ i) $ throws (i,m) ms
      | Nothing <- ms I.!? i = ms

throws :: (Int,Monkey) -> IntMap Monkey -> IntMap Monkey
throws (i,m) ms = foldl' throw (I.insert i (m { inspections = inspections m + length (items m) , items = [] }) ms) (items m)
   where
   throw :: IntMap Monkey -> Int -> IntMap Monkey
   throw ms' w = I.adjust (\m' -> m' { items = w' : items m' }) i' ms'
      where
      i'
         | rem w' (d $ mt m) == 0 = t $ mt m
         | otherwise = f $ mt m
      w' = div (num l `op` num r) 3
      (l,op,r) = operation m
      num n
         | Item x <- n = x
         | Old    <- n = w

-- part 2

part2 :: IntMap Monkey -> Int
part2 = mbusiness . (!! 10000) . iterate mround'

mround' :: IntMap Monkey -> IntMap Monkey
mround' = go 0
   where
   go i ms
      | Just m  <- ms I.!? i = go (succ i) $ throws' (i,m) ms
      | Nothing <- ms I.!? i = ms

throws' :: (Int,Monkey) -> IntMap Monkey -> IntMap Monkey
throws' (i,m) ms = foldl' throw (I.insert i (m { inspections = inspections m + length (items m) , items = [] }) ms) (items m)
   where
   deworry = product $ map (d . mt) $ I.elems ms
   throw :: IntMap Monkey -> Int -> IntMap Monkey
   throw ms' w = I.adjust (\m' -> m' { items = w' : items m' }) i' ms'
      where
      i'
         | rem w' (d $ mt m) == 0 = t $ mt m
         | otherwise = f $ mt m
      w' = mod (num l `op` num r) deworry
      (l,op,r) = operation m
      num n
         | Item x <- n = x
         | Old    <- n = w
