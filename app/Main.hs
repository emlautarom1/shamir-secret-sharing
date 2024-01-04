module Main where

import Control.Monad
import Data.Ratio
import System.Random
import System.Random.Shuffle

type Secret = Integer

type N = Int

type K = Int

type I = Int

type X = Integer

type Y = Ratio Integer

type Point = (X, Y)

type F = X -> Y

indexed :: [a] -> [(Int, a)]
indexed = zip [0 ..]

mkF :: [Integer] -> F
mkF coefs x =
  -- [65, 15]
  -- [65 * x ^ 0, 15 * x ^ 1]
  sum $ [(coef * x ^ i) % 1 | (i, coef) <- indexed coefs]

-- >>> mkF [65, 15] 0
-- 65 % 1
-- >>> mkF [65, 15] 1
-- 80 % 1
-- >>> mkF [65, 15] 2
-- 95 % 1
-- >>> mkF [65, 15] 3
-- 110 % 1
-- >>> mkF [65, 15] 4
-- 125 % 1

shamir :: Secret -> N -> K -> IO [Point]
shamir secret n k = do
  when (n < k) $ fail "n must be greater than or equal to k"

  let degree = k - 1
  coefs <- (secret :) <$> replicateM degree randomIO
  let polynomial = mkF coefs
  let points = [(x, polynomial x) | x <- fromIntegral <$> [1 .. n]]
  pure points

lagrange :: I -> [X] -> F
lagrange i xs x =
  product [(x - xj) % (xi - xj) | (j, xj) <- indexed xs, j /= i]
  where
    xi = xs !! i

-- >>> (lagrange 1 [1, 3]) 1
-- 0 % 1
-- >>> (lagrange 1 [1, 3]) 3
-- 1 % 1

reconstruct :: [Point] -> F
reconstruct points x = sum [yi * lagrange i xs x | (i, yi) <- indexed ys]
  where
    (xs, ys) = unzip points

recover :: [Point] -> Secret
recover points =
  let y = reconstruct points 0
   in if denominator y == 1
        then numerator y
        else error "failed to compute secret"

-- f(x) = 65 + 15x
-- deg(f) = 1
-- Required points = 2
-- Points = [(0, 65), (1, 80), (2, 95), (3, 110), (4, 125)]

-- >>> recover [(1, 80), (3, 110)]
-- 65
-- >>> recover [(2, 95), (4, 125)]
-- 65

main :: IO ()
main = do
  let secret = 1337
  let n = 5
  let k = 3
  putStrLn $ "secret: " <> show secret <> ", n: " <> show n <> ", k: " <> show k

  points <- shamir secret n k
  putStrLn $ "n points: " <> show points

  points' <- take k <$> shuffleM points
  putStrLn $ "k random points: " <> show points'

  let guess = recover points'
  putStrLn $ "guess: " <> show guess

  putStrLn $
    if guess == secret
      then "success!"
      else "failure!"
