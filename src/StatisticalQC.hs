module StatisticalQC where

import Control.Monad ()
import Control.Monad.State ( replicateM )
import RandomGen ( MonadGen(generate) )
import State (State)
import qualified State as S
import Test.HUnit (State, Test (..), Testable, assertBool, runTestTT, (~:), (~?=))
import Test.QuickCheck (Gen)

{-
Statistical Quick Check
-----------------------
The goal of this module is to provide an interface for testing
randomized algorithms. Since quick check only supports testing 
a variety of random inputs for properties that should _always_ 
hold, our statistical quick check will simply record the number of 
correct tests to record statistics for testing of randomized algs.
-}

-- TYPES -- 
data Statistics = Statistics
  { numTests :: Int,
    numCorrect :: Int,
    numTrueTests :: Int,
    numTrueCorrect :: Int,
    numFalseTests :: Int,
    numFalseCorrect :: Int
  }
  deriving (Show)

{-
Here are the functions for running statistical check on inputs
-}

-- stat check evaluates the value of the second tuple to count false instances
-- the (Bool, Bool) pairs represent (correct answer, whether our answer matches)
statCheck :: MonadGen m => Int -> m (Bool, Bool) -> m [(Bool, Bool)]
statCheck = replicateM

-- given a specific input that we want to verify, run randomized algorithm
-- on specific inputs for a given number of times
givenAnswerStatCheck :: MonadGen m => Int -> m Bool -> Bool -> m [(Bool, Bool)]
givenAnswerStatCheck n mx b = do
  bs <- replicateM n mx
  let cs = map (\x -> (b, b == x)) bs
  return cs

-- Given some testing function, record the error rate (useful for transitioning
-- into plotting)
errorCheck :: MonadGen m => m [(Bool, Bool)] -> m Float
errorCheck gen = do
  x <- gen
  let correct = foldr (\(a, b) acc -> if b then acc + 1 else acc) 0 x
  return (fromIntegral correct / fromIntegral (length x))

-- create a list of error rates given multiple tests we want to perform
errorList :: MonadGen m => [m [(Bool, Bool)]] -> m [Float]
errorList [] = pure []
errorList (g : gs) = do
  f <- errorCheck g
  fs <- errorList gs
  return (f : fs)

-- creates list of error rates for repeating the same test (to check 
-- consistency)
multiStatCheck :: MonadGen m => Int -> m [(Bool, Bool)] -> m [Float]
multiStatCheck n gen = replicateM n (errorCheck gen)

{-
Visualizing helpers!
Here are functions that help display or simplify the collection of 
statistics we are interested in collecting. Currently we only support 
collecting accuracy, accuracy for true test cases (tests where the 
true answer of the problem is TRUE), and accuracy for false test cases
(tests where the true answer of the problem is FALSE). 
-}

-- takes a generator of trial results and outputs
-- num trials, num passed trials, num trials w expected True and num such passed,
-- n num trials w expected False and num of such passed
statCollector :: [(Bool, Bool)] -> Statistics
statCollector xs =
  Statistics
    { numTests = length xs,
      numCorrect = length (filter snd xs),
      numTrueTests = length (filter fst xs),
      numTrueCorrect = length (filter (uncurry (&&)) xs),
      numFalseTests = length (filter (not . fst) xs),
      numFalseCorrect = length (filter (\x -> not (fst x) && snd x) xs)
    }

-- generate statistics after collecting stats from a helper
createStatistics :: MonadGen m => m [(Bool, Bool)] -> m Statistics
createStatistics = fmap statCollector

-- format statistics so it looks good
stringFormatter :: Int -> Int -> String
stringFormatter a b | b > 0 = show a ++ "/" ++ show b ++ " = " ++ show c ++ "%"
  where
    c = 100 * fromIntegral a / fromIntegral b
stringFormatter a b = show a ++ "/" ++ show b

-- print statistics
statPrinter :: Statistics -> IO ()
statPrinter s = do
  putStrLn ("Accuracy: " ++ stringFormatter (numCorrect s) (numTests s))
  putStrLn ("True Case Accuracy: " ++ stringFormatter (numTrueCorrect s) (numTrueTests s))
  putStrLn ("False Case Accuracy: " ++ stringFormatter (numFalseCorrect s) (numFalseTests s))

-- display function for IO interaction from a test
display :: (MonadGen m) => m [(Bool, Bool)] -> IO ()
display gen = generate (createStatistics gen) >>= statPrinter

{-
Command Line Testing Interface !!!!
This is the most exciting part of our module! Here we enact the 
testing that is done by QuickCheck in our own statistical quick check
module. We display the results after testing given a randomized and 
deterministic implementation. 

Note here that we have two main functions: statisticalChecker and 
approxChecker. This is because we've focused on two classes of randomized 
algorithms for this project: randomized verification and approximation. 
-}

-- given an instance x
-- checks if randomized algorithm r agrees with deterministic alg d
-- outputs (Expected Result, Whether r was correct)
statisticalChecker :: MonadGen m => (a -> m Bool) -> (a -> Bool) -> (a -> m (Bool, Bool))
statisticalChecker r d x = do
   bR <- r x
   let b = d x
   return (b, bR == b)

-- given an instance x, 
-- checks if randomized algorithm r achieves at least c * the score of d
-- outputs (True, whether r was c approx)
approxChecker :: MonadGen m => (a -> m Int) -> (a -> Int) -> Float -> (a -> m (Bool, Bool))
approxChecker r d c x = do
  a <- r x
  let b = d x
  return (True, fromIntegral a / fromIntegral b >= c)

checkRunner :: MonadGen m => (a -> m (Bool, Bool)) -> a -> Int -> IO ()
checkRunner f a n = display $ statCheck n (f a) 

runStatCheck :: MonadGen m => Int -> m a -> (a -> m Bool) -> (a -> Bool) -> IO ()
runStatCheck n a r d = do 
  g <- generate a 
  checkRunner (statisticalChecker r d) g n

runApproxCheck :: MonadGen m => Int -> m a -> (a -> m Int) -> (a -> Int) -> Float -> IO () 
runApproxCheck n a r d c = do 
  g <- generate a 
  checkRunner (approxChecker r d c) g n