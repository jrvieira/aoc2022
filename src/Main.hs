module Main where

import System.Environment

import Α01 qualified as Α ( main , test ) -- 01 Alpha
import Β02 qualified as Β ( main , test ) -- 02 Beta
import Γ03 qualified as Γ ( main , test ) -- 03 Gamma
import Δ04 qualified as Δ ( main , test ) -- 04 Delta
import Ε05 qualified as Ε ( main , test ) -- 05 Epsilon
import Ζ06 qualified as Ζ ( main , test ) -- 06 Zeta
import Η07 qualified as Η ( main , test ) -- 07 Eta
import Θ08 qualified as Θ ( main , test ) -- 08 Theta
import Ι09 qualified as Ι ( main , test ) -- 09 Iota
import Κ10 qualified as Κ ( main , test ) -- 10 Kappa
import Λ11 qualified as Λ ( main , test ) -- 11 Lambda
import Μ12 qualified as Μ ( main , test ) -- 12 Mu
import Ν13 qualified as Ν ( main , test ) -- 13 Nu
import Ξ14 qualified as Ξ ( main , test ) -- 14 Xi
import Ο15 qualified as Ο ( main , test ) -- 15 Omicron
import Π16 qualified as Π ( main , test ) -- 16 Pi
import Ρ17 qualified as Ρ ( main , test ) -- 17 Rho
import Σ18 qualified as Σ ( main , test ) -- 18 Sigma
import Τ19 qualified as Τ ( main , test ) -- 19 Tau
import Υ20 qualified as Υ ( main , test ) -- 20 Upsilon
import Φ21 qualified as Φ ( main , test ) -- 21 Phi
import Χ22 qualified as Χ ( main , test ) -- 22 Chi
import Ψ23 qualified as Ψ ( main , test ) -- 23 Psi
import Ω24 qualified as Ω ( main , test ) -- 24 Omega

data Day = Day { τ :: IO () , μ :: IO () }

day :: Int -> Day
day 01 = Day Α.test Α.main
day 02 = Day Β.test Β.main
day 03 = Day Γ.test Γ.main
day 04 = Day Δ.test Δ.main
day 05 = Day Ε.test Ε.main
day 06 = Day Ζ.test Ζ.main
day 07 = Day Η.test Η.main
day 08 = Day Θ.test Θ.main
day 09 = Day Ι.test Ι.main
day 10 = Day Κ.test Κ.main
day 11 = Day Λ.test Λ.main
day 12 = Day Μ.test Μ.main
day 13 = Day Ν.test Ν.main
day 14 = Day Ξ.test Ξ.main
day 15 = Day Ο.test Ο.main
day 16 = Day Π.test Π.main
day 17 = Day Ρ.test Ρ.main
day 18 = Day Σ.test Σ.main
day 19 = Day Τ.test Τ.main
day 20 = Day Υ.test Υ.main
day 21 = Day Φ.test Φ.main
day 22 = Day Χ.test Χ.main
day 23 = Day Ψ.test Ψ.main
day 24 = Day Ω.test Ω.main
day a = Day (error $ "invalid argument " ++ show a) undefined

main :: IO ()
main = do
   (n:m:_) <- (++ repeat "") <$> getArgs
   run (read n) m

run :: Int -> String -> IO ()
run n m
   | null m = μ $ day n
   | "test" <- m = τ $ day n
   | otherwise = error $ "invalid arguments: " ++ show n ++ " " ++ m
