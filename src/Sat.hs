module Sat (module Sat) where

--(Var (..), Valuation, CNF (..), Clause (..), Lit (..), satisfied, tRunRandomVerifierSat, tRunApproxChecker) where
import Control.Monad (liftM2, replicateM)
import qualified Control.Monad as QuickCheck.Gen
import Control.Monad.State (replicateM)
import qualified Data.List as List
import Data.Map (Map, insert)
import qualified Data.Map as Map
import Data.Ord ()
import RandomGen (Arb (arb), MonadGen (..), const', elements, listOf)
import qualified RandomGen
import State (State)
import qualified State as S
import StatisticalQC (display, runApproxCheck, statCheck)
import System.Random (StdGen)
import qualified System.Random as Random
import Test.HUnit (Test (..), assertBool, runTestTT, (~:), (~?=))
import Test.QuickCheck (Gen, Property, Testable (property), generate, oneof)
import qualified Test.QuickCheck as QuickCheck

{-
A 7/8 Approximation to SAT
---------------------------
SAT is np hard, but if we just randomly guess the valuation of
the literals, it is very likely that we get 7/8 of them correct !!
In this module, we implement the 7/8 approximation algorithm for SAT
and count the likelihood that we get such values correct.
-}

{-
TYPE DECLARATIONS
-}

-- | An expression in CNF (conjunctive normal form) is a conjunction
-- of clauses. We store these clauses in the conjunction in a list.
newtype CNF = Conj {clauses :: [Clause]} deriving (Eq, Ord, Show)

-- | A clause is a disjunction of a number of literals, again storing
-- each literal in a list.
newtype Clause = Disj {lits :: [Lit]} deriving (Eq, Ord, Show)

-- | A literal is either a positive or a negative variable
data Lit = Lit {polarity :: Bool, var :: Var} deriving (Eq, Ord, Show)

newtype Var = Var Char
  deriving (Eq, Ord, Show)

type Valuation = Map Var Bool

instance Semigroup CNF where
  Conj c1 <> Conj c2 = Conj (c1 <> c2)

instance Monoid CNF where
  mempty = Conj mempty

instance Semigroup Clause where
  Disj c1 <> Disj c2 = Disj (c1 <> c2)

instance Monoid Clause where
  mempty = Disj mempty

instance Enum Var where
  toEnum i = Var (toEnum (i + fromEnum 'A'))
  fromEnum (Var v) = fromEnum v - fromEnum 'A'

{-
Algorithms to evaluate Variables
-}

-- | A long list of variables
allVars :: [Var]
allVars = [svA ..]

emptyValuation :: Valuation
emptyValuation = Map.empty

fromList :: [(Var, Bool)] -> Valuation
fromList = Map.fromList

keys :: Valuation -> [Var]
keys = Map.keys

getVars :: CNF -> [Var]
getVars cnf = map var $ foldr (\c acc -> lits c ++ acc) [] (clauses cnf)

vars :: CNF -> [Var]
vars cnf = List.nub (List.sort $ getVars cnf)

{-
Helper functions to evaluate the performance of our randomized algos
-}

-- | counts number of clauses satisfied by valuation
score :: CNF -> Valuation -> Int
score (Conj clauses) v =
  foldr ((\b acc -> if b then 1 + acc else acc) . (`satisfied` v)) 0 clauses

satisfied :: Clause -> Valuation -> Bool
satisfied (Disj lits) v =
  foldr (\l acc -> Just (polarity l) == Map.lookup (var l) v || acc) False lits

{-
Randomized algorithms to evaluate SAT
-}

algoN :: MonadGen m => CNF -> Int -> m Valuation
algoN cnf n = do
  xs <- replicateM n (randomVal cnf)
  let (v, s) = foldr (\x acc -> if score cnf x > snd acc then (x, score cnf x) else acc) (emptyValuation, 0) xs
  return v

algoN' :: MonadGen m => Int -> CNF -> m Int
algoN' n c = do
  v <- algoN c n
  return (score c v)

-- creates random valuation for given CNF
randomVal :: MonadGen m => CNF -> m Valuation
randomVal cnf = do
  let l = vars cnf
  let n = length l
  bs <- listOf n (RandomGen.oneof [const' True, const' False])
  return (fromList (zip l bs))

-- function to find how many of the clauses are satisfied
randomVerifierN :: MonadGen m => m CNF -> Int -> m (Bool, Bool)
randomVerifierN gen n = do
  cnf@(Conj clauses) <- gen
  v <- algoN cnf n
  return (True, score cnf v >= (7 * length clauses) `div` 8)

{-
Randomized instances of variables
-}

-- | Generate a random variable (limited to the first `n` variables).
instance Arb Var where
  arb :: MonadGen m => Int -> m Var
  arb n | n < 1 = error "Must supply a positive number to genVar"
  arb n = elements (List.map toEnum [0 .. n -1])

-- | Generate a random literal with `n` distinct variables.
instance Arb Lit where
  arb :: MonadGen m => Int -> m Lit
  arb n = liftM2 Lit (elements [True, False]) (arb n)

-- | Generate a random Clause with `n` distinct variables.
instance Arb Clause where
  arb :: MonadGen m => Int -> m Clause
  arb n = do
    xs <- replicateM 3 (arb n)
    return (Disj xs)

-- | Generate a random CNF with `n` distinct variables.
instance Arb CNF where
  arb :: MonadGen m => Int -> m CNF
  arb n = do
    k <- arb n
    clauses <- listOf k (arb n)
    return (Conj clauses)

-- -- TESTING --

-- A few variables for test cases
svA, svB, svC, svD :: Var
svA = Var 'A'
svB = Var 'B'
svC = Var 'C'
svD = Var 'D'

{-
Randomized Tests
-}

-- instance of generator
qcArbCNF :: Int -> Test.QuickCheck.Gen CNF
qcArbCNF = arb :: Int -> Test.QuickCheck.Gen CNF

-- IO function for interactive testing
tSATRunner :: (Int -> Int -> Int -> IO ()) -> IO ()
tSATRunner f = do
  putStrLn "How many samples should we do?"
  input1 <- getLine
  putStrLn "How many variables?"
  input2 <- getLine
  putStrLn "How many valuations sampled per CNF?"
  input3 <- getLine
  let numRuns = (read input1 :: Int)
  let numVars = (read input2 :: Int)
  let numCalls = (read input3 :: Int)
  f numRuns numVars numCalls

-- counts on how many random inputs does our algo find a 87.5% valuation
tRandomVerifier :: Int -> Int -> Int -> IO ()
tRandomVerifier numRuns numVars numCalls = display $ statCheck numRuns (randomVerifierN (qcArbCNF numVars) numCalls)

tRunRandomVerifierSat :: IO ()
tRunRandomVerifierSat = tSATRunner tRandomVerifier

tRunApproxChecker :: IO ()
tRunApproxChecker = runApproxCheck 1000 (qcArbCNF 5) (algoN' 1) (\(Conj clauses) -> length clauses) 0.875