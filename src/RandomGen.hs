module RandomGen where

import Control.Monad ( replicateM )
import Data.Map (Map, insert)
import qualified State as S
import System.Random (StdGen, randomRIO)
import qualified System.Random as Random (mkStdGen, randomIO, uniform, uniformR)
import Test.HUnit (Test (..), assertBool, runTestTT, (~:), (~?=))
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Random (QCGen)
import qualified Gen
import Gen ( mkStdGen, bounded )

{-
Monadic Random Generation
-------------------------
The goal of this module is to create a monad that generates random samples.
The monad contains the implementation for the `bare bones' functionality of
a random generation process. The following declares the class of such a
random generation monad.
-}

class Monad m => MonadGen m where
  oneof :: [m a] -> m a
  generate :: m a -> IO a

instance MonadGen QuickCheck.Gen where
  oneof = QuickCheck.oneof
  generate = QuickCheck.generate

class Arb a where
  arb :: MonadGen m => Int -> m a

{-
Here are some functions that are relevant to our MonadGen implementation. 
-}

const' :: MonadGen m => a -> m a
const' = pure

listOf :: MonadGen m => Int -> m a -> m [a]
listOf = replicateM

elements :: MonadGen m => [a] -> m a
elements xs = oneof [const' x | x <- xs]

frequency :: MonadGen m => [(Int, m a)] -> m a
frequency [] = error "frequency called on empty list"
frequency table | any ((< 0) . fst) table = error "frequency called with negative weights"
frequency table | all ((== 0) . fst) table = error "frequency called with all zero weights"
frequency table = arb total >>= (`pick` table)
  where
    total = sum $ map fst table
    pick n ((k, x) : xs)
      | n <= k = x
      | otherwise = pick (n - k) xs
    pick _ _ = error "pick used with empty list"

sample :: (MonadGen m, Show a) => m a -> Int -> IO ()
sample gen i | i < 1 = return ()
sample gen i = do
  x <- generate gen
  print x
  sample gen (i - 1)


{-
Now we create instances of the MonadGen for IO, S.State StdGen. 
This creates a plug and play for our custom
generation and testing for randomized algorithms!! 
-}

instance MonadGen IO where
  oneof l = do
    let n = length l
    i <- randomRIO (0, n - 1)
    l !! i
  generate = id

instance MonadGen (S.State StdGen) where
  oneof l = do
    let n = length l
    i <- bounded n
    l !! i
  generate gen = do S.evalState gen . mkStdGen <$> (Random.randomIO :: IO Int)

{-
We also define a class of arbitrary instances of various types to use 
in our randomized algorithms. We use these for testing.
-}

instance Arb Int where
  arb i = oneof [const' j | j <- [0 .. i]]

instance Arb Bool where
  arb i = elements [False, True]

instance (Arb a, Arb b) => Arb (a, b) where
  arb i = (,) <$> arb i <*> arb i

instance (Arb a) => Arb [a] where
  arb i = frequency [(1, return []), (i, (:) <$> arb (i - 1) <*> arb (i - 1))]