module Λ11 where

import Zero.Zero

import Data.List.Split ( splitOn )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/11.txt"
   teqt "part 1" "" $ part1 input
-- teqt "part 2" undefined $ part2 input
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/11.txt"
   print $ part1 input
-- print $ part2 input
   pure ()

parse :: String -> [Monkey]
parse = map monkey . splitOn "\n\n"

data Monkey = Monkey {
   id :: Int , items :: [Int] ,
   operation :: (Item,Int -> Int -> Int,Item) ,
   mt :: Test }

data Item = Old | Item Int
data Test = Test { d :: Int , t :: Int , f :: Int }
   deriving ( Show )

instance Show Monkey where
   show (Monkey i its op mt@(Test td tt tf)) = unwords ["M",show i,show its,show mt]

monkey :: String -> Monkey
monkey = go . lines
   where
   go [i,its,op,td,tt,tf] = Monkey (head $ parseNums i) (parseNums its) (itemL op,pop op,itemR op) (Test (head $ parseNums td) (head $ parseNums tt) (head $ parseNums tf))
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

-- part 1

part1 :: [Monkey] -> String
part1 = show

worry = div 3

-- part 2

part2 :: a
part2 = undefined

