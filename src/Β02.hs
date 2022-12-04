module Β02 where

import Zero.Zero

import Data.Function ( on )

test :: IO ()
test = do
   input <- parse <$> readFile "./tests/02.txt"
   teqt "part 1" 15 $ part1 input
   teqt "part 2" 12 $ part2 input
   pure ()

main :: IO ()
main = do
   input <- parse <$> readFile "./input/02.txt"
   print $ part1 input
   print $ part2 input
   pure ()

parse :: String -> [Game]
parse = map (\[a,' ',b] -> uncurry Game $ (charToHand a,b)) . lines
   where
   charToHand :: Char -> Hand
   charToHand c
      | 'A' <- c = Rock
      | 'B' <- c = Paper
      | 'C' <- c = Scissors
      |  _  <- c = error "illegal hand"

data Game = Game Hand Char

-- part 1

data Hand = Rock | Paper | Scissors
   deriving ( Eq, Enum, Bounded )

instance Ord Hand where
   compare Rock Scissors = GT
   compare Scissors Rock = LT
   compare a b = on compare fromEnum a b

outcome :: Game -> Int
outcome (Game a b)
   | GT <- r = 0
   | EQ <- r = 3
   | LT <- r = 6
   where
   r = compare a (h b)

h :: Char -> Hand
h c
   | 'X' <- c = Rock
   | 'Y' <- c = Paper
   | 'Z' <- c = Scissors
   |  _  <- c = error "illegal hand"

score :: Game -> Int
score g@(Game _ b) = succ (fromEnum (h b)) + outcome g

part1 :: [Game] -> Int
part1 = sum . map score

-- part 2

part2 :: [Game] -> Int
part2 = sum . map strategy

strategy :: Game -> Int
strategy (Game a b)
   | 'X' <- b = 0 + (succ $ mod (fromEnum $ back a) 3)
   | 'Y' <- b = 3 + (succ $ mod (fromEnum        a) 3)
   | 'Z' <- b = 6 + (succ $ mod (fromEnum $ forw a) 3)
   |  _  <- b = error "illegal strategy"
