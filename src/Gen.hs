{-
---
Adapted from "In class exercise: Random Generation"
---
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gen where
import Control.Monad
import qualified State as S
import System.Random (StdGen)
import qualified System.Random as Random (mkStdGen, randomIO, uniform, uniformR)

{-
Random Generation
-----------------
The goal of this module is to use it for the implementation of MonadGen
-}

mkStdGen :: Int -> StdGen
mkStdGen = Random.mkStdGen . (* (3 :: Int) ^ (20 :: Int))

type Gen a = S.State StdGen a

class GenArb a where
  genArb :: Gen a

instance GenArb Int where
  genArb = do
    s <- S.get
    let (y :: Int, s') = Random.uniform s
    S.put s'
    return y

bounded :: Int -> Gen Int
bounded b = (`mod` b) <$> genArb

genSample :: Show a => Gen a -> IO ()
genSample gen = do
  seed <- (Random.randomIO :: IO Int) -- get a seed from the global random number generator
  -- hidden in the IO monad
  let vals = S.evalState (replicateM 10 gen) (mkStdGen seed)
  forM_ vals print

instance (GenArb a, GenArb b) => GenArb (a, b) where
  genArb = (,) <$> genArb <*> genArb

genElements :: [a] -> Gen a
genElements xs = do
  b <- bounded (length xs)
  return (xs !! b)

instance GenArb Bool where
  genArb = genElements [False, True]

genFrequency :: [(Int, Gen a)] -> Gen a
genFrequency table = do
  let total = sum (map fst table)
  index <- bounded total
  nth index table

nth :: Int -> [(Int, a)] -> a
nth i ((j, x) : xs)
  | i < j = x
  | otherwise = nth (i - j) xs
nth _ [] = error "frequency: empty"

instance (GenArb a) => GenArb [a] where
  genArb = genFrequency [(1, return []), (3, (:) <$> genArb <*> genArb)]