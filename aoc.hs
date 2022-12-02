import System.Environment
import System.Process

-- import Data.Functor ( (<&>) )
-- import Control.Monad ( (>=>) )

default (Int)

main :: IO ()
main = do
   sess <- init <$> readFile "session.txt"
   (k:args) <- (++ repeat "") <$> getArgs
   f sess k args

data Get = Day | Year

f :: String -> String -> [String] -> IO ()
f sess k
   | "open"  <- k = open
   | "fetch" <- k = fetch
   | "test"  <- k = test
   | "run"   <- k = run
   | "help"  <- k = const help
   | otherwise = const $ putStrLn ("not a command: aoc " ++ k ++ "\ntype \"aoc help\"")

   where

   help :: IO ()
   help = do
      putStrLn "  "
      putStrLn "  \x1b[0;33m# open daily challenge\x1b[0m"
      putStrLn "  aoc open {d} [{y}]"
      putStrLn "  "
      putStrLn "  \x1b[0;33m# fetch daily input\x1b[0m"
      putStrLn "  aoc fetch {d} [{y}]"
      putStrLn "  "
      putStrLn "  \x1b[0;33m# daemonize daily tests\x1b[0m"
      putStrLn "  aoc test {d}"
      putStrLn "  "
      putStrLn "  \x1b[0;33m# compile and run solution\x1b[0m"
      putStrLn "  aoc run {d}"
      putStrLn "  "

   now :: Get -> IO String
   now Day  = head . words <$> readCreateProcess (shell "echo $(date -d \"\" '+%_d')") ""
   now Year = head . words <$> readCreateProcess (shell "echo $(date -d \"\" '+%Y')") ""

   -- open

   open :: [String] -> IO ()
   open ~(d:y:x:_) = do
      ny <- now Year
      val ny
      where
      val ny
         | null d || not (null x) = putStrLn $ "format: aoc open {d} [{y}]"
         | elem d (map show [1..25]) && null y = go d ny
         | elem d (map show [1..25]) && elem y (map show [2015..read ny]) = go d y
         | otherwise = putStrLn $ "invalid arguments: " ++ unwords [d,y]
      go :: String -> String -> IO ()
      go vd vy = do
         system $ "w3m https://adventofcode.com/" ++ vy ++ "/day/" ++ vd
         pure ()

   -- run

   run :: [String] -> IO ()
   run ~(d:x:_)
      | null d || not (null x) = putStrLn $ "format: aoc run {d}"
      | elem (read d) [1..25] = go
      | otherwise = putStrLn $ "invalid argument: " ++ d
      where
      go :: IO ()
      go = do
         pwd <- init <$> readCreateProcess (shell "basename $PWD") ""
         _ <- system $ "cabal run -v0 -O2 " ++ pwd ++ " " ++ d
         pure ()

   -- test

   test :: [String] -> IO ()
   test ~(d:x:_)
      | null d || not (null x) = putStrLn $ "format: aoc test {day}"
      | elem d (map show [1..25]) = go
      | otherwise = putStrLn $ "invalid argument: " ++ d
      where
      go :: IO ()
      go = do
         _ <- system $ unwords
            [ "ghcid"
            , "--warnings"
         -- , "--lint"
            , "--no-status"
            , "--clear"
            , "--no-height-limit"
            , "--test-message=\"\n\""
            , "--test=\":main " ++ d ++ " test\""
            ]
         pure ()

   -- fetch

   fetch :: [String] -> IO ()
   fetch ~(d:y:x:_) = do
      ny <- now Year
      val ny
      where
      val :: String -> IO ()
      val ny
         | null d || not (null x) = putStrLn $ "format: aoc fetch {d} [{y}]"
         | elem d (map show [1..25]) && null y = go d ny
         | elem d (map show [1..25]) && elem y (map show [2015..read ny]) = go d y
         | otherwise = putStrLn $ "invalid arguments: " ++ unwords [d,y]
      go :: String -> String -> IO ()
      go vd vy = do
         let url = "https://adventofcode.com/" ++ vy ++ "/day/" ++ vd
         let ifile = "./input/" ++ pad 2 '0' vd ++ ".txt"
         i <- readCreateProcess (shell $ unwords
            [ "curl -s --cookie \"session=" ++ sess ++ "\""
            , url ++ "/input"
            , "-o \"" ++ ifile ++ "\" --create-dirs"
            , "&& cat " ++ ifile
            ]) ""
         putStrLn i

   pad :: Int -> Char -> String -> String
   pad n c s = replicate (max 0 $ n - length s) c ++ s

