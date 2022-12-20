module Ν13 where

import Zero.Zero

import Data.Char ( isDigit )
import Data.List ( sort )
import Data.List.Split ( splitOn )
import Data.IntMap ( IntMap )
import Data.IntMap qualified as I

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/13.txt"
   teqt "part 1" 13 $ part1 input
   teqt "part 2" 140 $ part2 input
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/13.txt"
   print $ part1 input
   print $ part2 input
   pure ()

parse :: String -> IntMap (Pair Packet)
parse = I.fromList . zip [1..] . map (pair . map tree . lines) . splitOn "\n\n"

type Pair a = (a,a)

pair :: Show a => [a] -> Pair a
pair [a,b] = (a,b)
pair e = error $ unwords ["unexpected input",show e]

data Packet = Int Int | List [Packet]
   deriving ( Read, Eq )

instance Show Packet where
   show (List l) = show l
   show (Int  i) = show i

tree :: String -> Packet
tree = read . go
   where
   go "" = ""
   go (c:cs)
      | '['  <- c = "List [" <> go cs
      | isDigit c = let (int,rest) = span isDigit (c:cs) in "Int " <> int <> go rest
      | otherwise = c : go cs

-- part 1

part1 :: IntMap (Pair Packet) -> Int
part1 = sum . I.keys . I.filter (uncurry (<=))

instance Ord Packet where
   compare (Int  a) (Int  b) = compare a b
   compare (List a) (List b) = compare a b
   compare (List a) (Int  b) = compare (List a) (List [Int b])
   compare (Int  a) (List b) = compare (List [Int a]) (List b)

-- part 2

part2 :: IntMap (Pair Packet) -> Int
part2 = product . I.keys . I.filter (∈ dp) . I.fromList . zip [1..] . sort . (dp <>) . concatMap unpair . I.elems
   where
   dp = [
      List [List [Int 2]] ,
      List [List [Int 6]] ]

unpair :: Pair a -> [a]
unpair (a,b) = [a,b]
