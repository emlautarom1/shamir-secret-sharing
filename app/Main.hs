{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Concurrent
import Control.Monad
import Data.Data
import Data.Modular
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Dispatch.Dynamic
import Effectful.Fail
import Effectful.State.Dynamic
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

data Msg = Msg {from :: Int, at :: X, value :: Y}
  deriving (Show, Eq)

node :: (Coms :> es, Leak Y :> es, Fail :> es, IOE :> es) => N -> K -> Int -> Eff es Point
node n k me = do
  let me' = toMod @P (fromIntegral me)

  -- Compute [f_me(0), f_me(1), ..., f_me(n)]
  mySecret <- liftIO $ randomRIO (1, p - 1)
  myPoints <- liftIO $ shamir mySecret n k

  -- For testing, we globally store f(0) = f_me(0) + f_me(1) + ... + f_me(n)
  leak $ toMod @P mySecret

  -- To each participant i, send f_me(i)
  forM_ [1 .. n] $ \to -> do
    let (x, y) = myPoints !! (to - 1)
    sendMsg to $ Msg {from = me, at = x, value = y}

  -- Wait for each participant i to send f_i(me)
  f_ns_atMe <- replicateM n $ do
    msg <- recvMsg
    when (at msg /= me') $ do
      fail $ "participant " <> show (from msg) <> " sent a message at " <> show (at msg) <> " instead of " <> show me
    pure $ value msg

  -- Compute f(me) = f_1(me) + f_2(me) + ... + f_n(me)
  let f_AtMe = sum f_ns_atMe
  return (me', f_AtMe)

dkg :: IO ()
dkg = do
  let n = 5
  let k = 3

  putStrLn $ "n: " <> show n <> ", k: " <> show k

  channels :: [Chan Msg] <- replicateM n newChan
  (sharedPoints :: [Point], sharedSecret :: Y) <- runEff . runFailIO . runLeak 0 . runConcurrent $
    forConcurrently [1 .. n] $ \me ->
      runComsChannels channels me $ node n k me

  when (length sharedPoints /= n) $ fail "not enough points"

  sharedPoints' <- take k <$> shuffleM sharedPoints
  putStrLn $ "k random points: " <> show sharedPoints'

  -- With k points (i, f(i)) where i = 1, 2, ..., n, we can reconstruct f(0)
  let guess = recover sharedPoints'
  putStrLn $ "guess: " <> show guess

  putStrLn $
    if guess == unMod sharedSecret
      then "success!"
      else "failure!"

----------------------------------------
-- Effectful

data Coms :: Effect where
  SendMsg :: Int -> Msg -> Coms m ()
  RecvMsg :: Coms m Msg

type instance DispatchOf Coms = 'Dynamic

sendMsg :: Coms :> es => Int -> Msg -> Eff es ()
sendMsg to msg = send (SendMsg to msg)

recvMsg :: Coms :> es => Eff es Msg
recvMsg = send RecvMsg

runComsChannels :: IOE :> es => [Chan Msg] -> Int -> Eff (Coms : es) a -> Eff es a
runComsChannels channels me = interpret $ \_ -> \case
  SendMsg to msg -> do
    let toChannel = channels !! (to - 1)
    liftIO $ writeChan toChannel msg
  RecvMsg -> do
    let myChannel = channels !! (me - 1)
    liftIO $ readChan myChannel

data Leak a :: Effect where
  Leak :: a -> Leak a m ()

type instance DispatchOf (Leak a) = 'Dynamic

leak :: Leak a :> es => a -> Eff es ()
leak a = send (Leak a)

runLeak :: Num s => s -> Eff (Leak s : es) a -> Eff es (a, s)
runLeak s0 = reinterpret (runStateShared s0) $ \_ -> \case
  Leak a -> modify (+ a)

----------------------------------------
-- Main

main :: IO ()
main = dkg
