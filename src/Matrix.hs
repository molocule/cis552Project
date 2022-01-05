module Matrix (module Matrix) where

--(Vector, Matrix, tRunTrueMatrixTest, tRunFalseMatrixTest, tRunStatCheckerQC, tRunRandomVerifierCorrect, tErrorList, tRunRandomVerifierTest) where

import Control.Monad (liftM3, replicateM)
import qualified Control.Monad as QuickCheck.Gen
import Control.Monad.State (replicateM)
import Data.Map (Map, insert)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import RandomGen (Arb (..), MonadGen (..), const', listOf, sample)
import qualified RandomGen
import State (State)
import qualified State as S
import StatisticalQC (display, errorList, givenAnswerStatCheck, runStatCheck, statCheck)
import System.Random (StdGen)
import qualified System.Random as Random
import Test.HUnit (Test (..), assertBool, runTestTT, (~:), (~?=))
import Test.QuickCheck (Gen, Property, Testable (property), generate, oneof)
import qualified Test.QuickCheck as QuickCheck

{-
Randomized Matrix Verification
------------------------------
This module creates the types and implementation for randomized matrix verification.
The key idea of the algorithm is ``fingerprinting" or generating a random vector r
to see whether ABr = Cr for matrices A, B, C.

The highlights of our implementation include the MonadGen and Statistical QC components
-}

-- DECLARE TYPES

newtype Vector = V {v :: [Int]} deriving (Show)

newtype Matrix = M {vs :: [Vector]} deriving (Show)

instance Eq Vector where
  (==) :: Vector -> Vector -> Bool
  V v == V v' = (==) v v'

instance Eq Matrix where
  (==) :: Matrix -> Matrix -> Bool
  M m == M m' = (==) m m'

-- MATRIX OPERATIONS

vectorLen :: Vector -> Int
vectorLen (V v) = length v

matDim :: Matrix -> (Int, Int)
matDim (M m) = (length m, num)
  where
    num = foldr (max . vectorLen) 0 m

isSquare :: Matrix -> Bool
isSquare a =
  let (m, n) = matDim a
   in m == n

dimCompat :: Matrix -> Matrix -> Bool
dimCompat m1 m2 = snd (matDim m1) == fst (matDim m2)

vectorMatCompat :: Matrix -> Vector -> Bool
vectorMatCompat m v = snd (matDim m) == vectorLen v

-- Check that matricese are square and of the same dimension
matCompat :: Matrix -> Matrix -> Bool
matCompat m m' =
  let bothSquare = isSquare m && isSquare m'
   in bothSquare && dimCompat m m'

dot :: Vector -> Vector -> Int
dot (V v) (V v') = sum $ zipWith (*) v v'

transposeVector :: Vector -> Matrix
transposeVector (V v) = M $ map (\x -> V [x]) v

vHead :: Vector -> Maybe Int
vHead (V []) = Nothing
vHead (V [v]) = Just v
vHead (V (v : vs)) = Just v

vTail :: Vector -> Vector
vTail (V (v : vs)) = V vs
vTail (V []) = V []

-- transpose only works for square matrices and truncates otherwise
transpose :: Matrix -> Matrix
transpose (M []) = M []
transpose (M [V []]) = M []
transpose (M [_, V []]) = M []
transpose (M ((V []) : xs)) = M []
transpose (M [_, V [], _]) = M []
transpose (M [x]) = transposeVector x
transpose (M x) =
  let M t = transpose (M (map vTail x))
   in let h = f $ map vHead x
       in M (V h : t)
  where
    f :: [Maybe a] -> [a]
    f [] = []
    f (Nothing : xs) = []
    f (Just x : xs) = x : f xs

-- if the dimensions are wrong, return empty matrix
-- here, we include a deterministic implementation
-- that will be helpful in quickCheck
matrixMult :: Matrix -> Matrix -> Matrix
matrixMult (M m) (M m')
  | dimCompat (M m) (M m') =
    M
      ( map
          ( matrixVectorMult (transpose (M m'))
          )
          m
      )
matrixMult m m' = error "Incompatible Matrices"

matrixEval :: (Matrix, Matrix, Matrix) -> Bool
matrixEval (m1, m2, m3) = m1 `matrixMult` m2 == m3

matrixVectorMult :: Matrix -> Vector -> Vector
matrixVectorMult (M rows) v | vectorMatCompat (M rows) v = V $ map (dot v) rows
matrixVectorMult m v = error "Incompatible Matrix Vector"

-- Random instance of vector

instance Arb Vector where
  arb :: MonadGen m => Int -> m Vector
  arb i = fmap V (listOf i arb')
    where
      arb' = RandomGen.oneof [const' j | j <- [0 .. 1]]

-- Random instance of matrix

instance Arb Matrix where
  arb :: MonadGen m => Int -> m Matrix
  arb n = do
    vs <- replicateM n (arb n)
    return (M vs)

{-
Randomized Matrix generation
-}

tripletMatrix :: MonadGen m => m Matrix -> m (Matrix, Matrix, Matrix)
tripletMatrix gen = liftM3 (,,) gen gen gen

{-
Randomized Algorithm for Matrix verification
-}

dimCheck :: Matrix -> Matrix -> Matrix -> Vector -> Bool
dimCheck m1 m2 m3 v = matCompat m1 m2 && matCompat m2 m3 && vectorMatCompat m1 v

-- Randomized verifier algorithm takes in matrices A, B, C
-- Returns whether A*B = C
vectorCheck :: Matrix -> Matrix -> Matrix -> Vector -> Bool
vectorCheck a b c v
  | dimCheck a b c v =
    matrixVectorMult
      a
      ( matrixVectorMult b v
      )
      == matrixVectorMult c v
vectorCheck a b c v = error "Wrong dimensions"

-- Vector check with random vector
verifier :: MonadGen m => m Vector -> (Matrix, Matrix, Matrix) -> m Bool
verifier gen (a, b, c) = fmap (vectorCheck a b c) gen

-- repeats verifier process n times
-- returns whether A*B = C
verifierN :: MonadGen m => Int -> m Vector -> (Matrix, Matrix, Matrix) -> m Bool
verifierN n gen (a, b, c) = do
  bs <- replicateM n (verifier gen (a, b, c))
  return (and bs)

-- performs verifierN for n iterations on random matrices
-- returns (whether A*B = C, whether randomized answer matches correct answer)
randomVerifierN :: MonadGen m => m Matrix -> m Vector -> Int -> m (Bool, Bool)
randomVerifierN gm gv n = do
  m1 <- gm
  m2 <- gm
  m3 <- gm
  let b = matrixMult m1 m2 == m3 -- b = correct value
  bs <- verifierN n gv (m1, m2, m3)
  return (b, b == bs)

randomVerifierCorrectMult :: MonadGen m => m Matrix -> m Vector -> Int -> m (Bool, Bool)
randomVerifierCorrectMult gm gv n = do
  m1 <- gm
  m2 <- gm
  let m3 = matrixMult m1 m2
  bs <- verifierN n gv (m1, m2, m3)
  return (True, bs)

-- {-
-- MATRIX TESTING
-- --------------
-- Randomized Test Suite for matrix verification
-- -}

-- Vectors used for testing

mvector3 :: Vector
mvector3 = V [1, 2]

mvector4 :: Vector
mvector4 = V [1, 3]

mvector5 :: Vector
mvector5 = V [2, 4]

mvector8 :: Vector
mvector8 = V [4, 5]

matrix1 :: Matrix
matrix1 = M [mvector3, mvector4]

matrix2 :: Matrix
matrix2 = M [mvector5, mvector8]

matrix3 :: Matrix
matrix3 = M [V [10, 14], V [14, 19]]

{-
Randomized Matrix Testing
-}

-- Here is a list of all of the instances of MonadGen
arbQCv :: Int -> Test.QuickCheck.Gen Vector
arbQCv = arb :: Int -> Test.QuickCheck.Gen Vector

arbQCm :: Int -> Test.QuickCheck.Gen Matrix
arbQCm = arb :: Int -> Test.QuickCheck.Gen Matrix

arbIOv :: Int -> IO Vector
arbIOv = arb :: Int -> IO Vector

arbIOm :: Int -> IO Matrix
arbIOm = arb :: Int -> IO Matrix

arbSTv :: Int -> S.State StdGen Vector
arbSTv = arb :: Int -> S.State StdGen Vector

arbSTm :: Int -> S.State StdGen Matrix
arbSTm = arb :: Int -> S.State StdGen Matrix

{-
Here are the actual test cases that we can run!
First we have the IO framework for running the tests.
These take in tests that take number of test and size
parameters and then output some IO implementation.
-}

testRunner :: (Int -> Int -> IO ()) -> IO ()
testRunner test = do
  putStrLn "How many runs should we do?"
  input1 <- getLine
  putStrLn "What size of matrix?"
  input2 <- getLine
  let numRuns = (read input1 :: Int)
  let size = (read input2 :: Int)
  test numRuns size

testSpecificMatrices :: (Int -> IO ()) -> IO ()
testSpecificMatrices f = do
  putStrLn "How many runs should we do?"
  input1 <- getLine
  let numRuns = (read input1 :: Int)
  f numRuns

{-
These are the specific tests and how to run them
-}

givenMatrixTest :: MonadGen m => Matrix -> Matrix -> Matrix -> (Int -> m Vector) -> Int -> IO ()
givenMatrixTest m1@(M vs) m2 m3 a numRuns = do
  let evaluated = matrixEval (m1, m2, m3)
   in display $ givenAnswerStatCheck numRuns (verifier (a (length vs)) (m1, m2, m3)) evaluated

trueMatrixTest :: Int -> IO ()
trueMatrixTest = givenMatrixTest matrix1 matrix2 matrix3 arbQCv

tRunTrueMatrixTest :: IO ()
tRunTrueMatrixTest = testSpecificMatrices trueMatrixTest

falseMatrixTest :: Int -> IO ()
falseMatrixTest = givenMatrixTest matrix1 matrix2 matrix1 arbQCv

tRunFalseMatrixTest :: IO ()
tRunFalseMatrixTest = testSpecificMatrices falseMatrixTest

statCheckerQC :: Int -> Int -> IO ()
statCheckerQC numRuns size = runStatCheck numRuns (tripletMatrix (arbQCm size)) (verifierN 1 (arbQCv size)) matrixEval

tRunStatCheckerQC :: IO ()
tRunStatCheckerQC = testRunner statCheckerQC

randomVerifierCorrect :: Int -> Int -> IO ()
randomVerifierCorrect numRuns size = display $ statCheck numRuns (randomVerifierCorrectMult (arbQCm size) (arbQCv size) 1)

tRunRandomVerifierCorrect :: IO ()
tRunRandomVerifierCorrect = testRunner randomVerifierCorrect

randomVerifierTest :: Int -> Int -> IO ()
randomVerifierTest numRuns size = display $ statCheck numRuns (randomVerifierN (arbQCm size) (arbQCv size) 1)

tRunRandomVerifierTest :: IO ()
tRunRandomVerifierTest = testRunner randomVerifierTest

{-
Finally, here is a test that allows us to track the error as size
of the matrix increases.
-}

gs :: Gen [Float]
gs = errorList [replicateM 1000 (randomVerifierN (arbQCm size) (arbQCv size) 1) | size <- [1 .. 10]]

tErrorList :: IO ()
tErrorList = RandomGen.generate gs >>= print