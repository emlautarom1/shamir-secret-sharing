{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Data
import Data.Modular
import GHC.TypeLits
import System.Random
import System.Random.Shuffle

type Secret = Integer

type P = (2 ^ 257 - 93)

p :: Integer
p = fromIntegral $ natVal (Proxy :: Proxy P)

type N = Int

type K = Int

type I = Int

type X = Integer / P

type Y = Integer / P

type Point = (X, Y)

type F = X -> Y

indexed :: [a] -> [(Int, a)]
indexed = zip [0 ..]

-- | `Horner's Rule` for polynomial evaluation
horner :: [Integer / P] -> F
horner coefs x =
  -- Equivalent to:
  -- sum $ [coef * x ^ i | (i, coef) <- indexed coefs]
  -- Ex:
  -- coefs = [65, 15]
  -- horner coefs = \x -> (65 * x ^ 0) + (15 * x ^ 1)
  foldr (\a !acc -> a + acc * x) 0 coefs

-- >>> horner [65, 15] 0
-- 65
-- >>> horner [65, 15] 1
-- 80
-- >>> horner [65, 15] 2
-- 95
-- >>> horner [65, 15] 3
-- 110
-- >>> horner [65, 15] 4
-- 125

shamir :: Secret -> N -> K -> IO [Point]
shamir secret n k = do
  when (n < k) $ fail "n must be greater than or equal to k"

  let degree = k - 1
  coefs <- (secret :) <$> replicateM degree (randomRIO (1, p - 1))
  let polynomial = horner $ map (toMod @P) coefs
  let points = [(x, polynomial x) | x <- fromIntegral <$> [1 .. n]]
  pure points

lagrange :: I -> [X] -> F
lagrange i xs x =
  product [(x - xj) / (xi - xj) | (j, xj) <- indexed xs, j /= i]
  where
    xi = xs !! i

-- >>> (lagrange 1 [1, 3]) 1
-- 0
-- >>> (lagrange 1 [1, 3]) 3
-- 1

reconstruct :: [Point] -> F
reconstruct points x =
  sum [yi * lagrange i xs x | (i, yi) <- indexed ys]
  where
    (xs, ys) = unzip points

recover :: [Point] -> Secret
recover points = unMod $ reconstruct points 0

-- f(x) = 65 + 15x
-- deg(f) = 1
-- Required points = 2
-- Points = [(0, 65), (1, 80), (2, 95), (3, 110), (4, 125)]

-- >>> recover [(1, 80), (3, 110)]
-- 65
-- >>> recover [(2, 95), (4, 125)]
-- 65

----------------------------------------
-- Shamir Secret Sharing

sss :: IO ()
sss = do
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

----------------------------------------
-- DKG

data Msg = Msg {from :: Int, at :: Integer / P, value :: Integer / P}
  deriving (Show, Eq)

dkg :: IO ()
dkg = do
  let n = 5
  let k = 3

  putStrLn $ "n: " <> show n <> ", k: " <> show k

  sharedSecret :: MVar (Integer / P) <- newMVar (toMod @P 0)
  channels :: [Chan Msg] <- replicateM n newChan
  sharedPoints :: [Point] <- forConcurrently [1 .. n] $ \me -> do
    let me' = toMod @P (fromIntegral me)
    let myChannel = channels !! (me - 1)

    mySecret <- randomRIO (1, p - 1)
    myPoints <- shamir mySecret n k

    modifyMVar_ sharedSecret $ \acc -> do
      return $ acc + toMod mySecret

    forM_ [1 .. n] $ \to -> do
      let (x, y) = myPoints !! (to - 1)
      let toChannel = channels !! (to - 1)
      writeChan toChannel $ Msg {from = me, at = x, value = y}

    f_me_atMe <- replicateM n $ do
      msg <- readChan myChannel
      when (at msg /= me') $ do
        fail $ "participant " <> show (from msg) <> " sent a message at " <> show (at msg) <> " instead of " <> show me
      pure $ value msg

    let f_AtMe = sum f_me_atMe
    return (me', f_AtMe)

  when (length sharedPoints /= n) $ fail "not enough points"

  sharedPoints' <- take k <$> shuffleM sharedPoints
  putStrLn $ "k random points: " <> show sharedPoints'

  let guess = recover sharedPoints'
  putStrLn $ "guess: " <> show guess

  sharedSecret' <- unMod <$> takeMVar sharedSecret
  putStrLn $
    if guess == sharedSecret'
      then "success!"
      else "failure!"

main :: IO ()
main = dkg
