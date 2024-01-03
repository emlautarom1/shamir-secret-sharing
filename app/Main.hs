{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad
import Data.Ratio
import System.Random
import System.Random.Shuffle (shuffleM)

type Secret = Integer

type N = Int

type K = Int

type I = Int

type X = Integer

type Y = Rational

type Point = (Integer, Integer)

type F = X -> Y

indexed :: [a] -> [(Int, a)]
indexed = zip [0 ..]

except :: [a] -> Int -> ([a], a)
xs `except` i = foldr f ([], error "index out of bounds") (indexed xs)
  where
    f :: (Int, a) -> ([a], a) -> ([a], a)
    f (idx, x) (xs', x') = if idx == i then (xs', x) else (x : xs', x')

-- >>> [0 .. 9] `except` 9
-- ([0,1,2,3,4,5,6,7,8],9)

compute :: [Integer] -> X -> Integer
compute coefs x =
  -- [65, 15]
  -- [65 * x ^ 0, 15 * x ^ 1]
  sum $ [coef * x ^ i | (i, coef) <- indexed coefs]

-- >>> compute [65, 15] 0
-- 65
-- >>> compute [65, 15] 1
-- 80
-- >>> compute [65, 15] 2
-- 95
-- >>> compute [65, 15] 3
-- 110
-- >>> compute [65, 15] 4
-- 125

shamir :: Secret -> N -> K -> IO [Point]
shamir secret n k = do
  when (n < k) $ fail "n must be greater than or equal to k"

  let degree = k - 1
  coefs <- (secret :) <$> replicateM degree randomIO
  let points = [(x, compute coefs x) | x <- fromIntegral <$> [1 .. n]]

  pure points

lagrange :: [X] -> I -> F
lagrange xs i x =
  product [(x - xj) % (xi - xj) | xj <- xs']
  where
    (xs', xi) = xs `except` i

-- >>> (lagrange [1, 3] 1) 1
-- 0 % 1
-- >>> (lagrange [1, 3] 1) 3
-- 1 % 1

reconstruct :: [Point] -> F
reconstruct points x = sum [term i | i <- [0 .. k - 1]]
  where
    term i = ((ys !! i) % 1) * lagrange xs i x
    (xs, ys) = unzip points
    k = length points

reveal :: [Point] -> Secret
reveal points =
  let y = reconstruct points 0
   in if denominator y == 1
        then numerator y
        else error "failed to compute secret"

-- f(x) = 65 + 15x
-- deg(f) = 1
-- Required points = 2
-- Points = [(0, 65), (1, 80), (2, 95), (3, 110), (4, 125)]

-- >>> reveal [(1, 80), (3, 110)]
-- 65
-- >>> reveal [(2, 95), (4, 125)]
-- 65

main :: IO ()
main = do
  let secret = 1337
  let n = 3
  let k = 2
  putStrLn $ "secret: " <> show secret <> ", n: " <> show n <> ", k: " <> show k

  points <- shamir secret n k
  -- putStrLn $ "n points: " <> show points

  points' <- take k <$> shuffleM points
  -- putStrLn $ "k random points: " <> show points'

  let guess = reveal points'
  putStrLn $ "guess: " <> show guess

  putStrLn $
    if guess == secret
      then "success!"
      else "failure!"
