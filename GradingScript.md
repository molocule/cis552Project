# CIS 552 Final Project Grading Script

- What are the main modules of your program? Why did you divide them up in this way?
  - The main Haskellic modules of our program are the RandomGen monad and the Statistical Quick Check functionality
    - In _RandomGen_, we describe a random generation monad that randomly generates samples of a given form. We also give three instances of this monad using QuickCheck.Gen, IO, and the random State monad covered in class.
    - In _StatisticalQC_, we define a function that can check whether a randomized algorithm works in _expectation_. While QuickCheck is great for finding counterexamples to properties that s…
    - The last two modules are where we implemented the algorithms In _Matrix_, we build out the Matrix and Vector type classes, and our matrix multiplication verification samples random vectors from an instance of MonadGen. In _Sat_, we copied much of the type structure from our homework. The algorithm sampled random valuations, computed their scores (i.e. # of clauses satisfied), and output the valuation with the best score.
  - Two modules load in the State/RandomGen monad we covered in class
  - The remaining modules implement our two randomized algorithms
    - Matrix Verification: receives input matrices A, B, C; uses random vectors to test whether A*B = C by checking if A * (Bv) = Cv for random v
    - 3SAT Approximation: receives 3-SAT CNF C; samples random valuations v and picks the best to achieve a ⅞ approximation with high probability
- Are there any parts of the code that you are particularly proud of? Where did you spend the most time polishing your implementation? What did it look like before? (In preparation for this question, you may want to have the the old version in comments available for comparison.)
  _ We spent the most time writing and rewriting the RandomGen monad. We are particularly proud of how we made it a monad and created instances of other generators with it such as IO and QuickCheck.
  _ In previous iterations, we had this:

          ```type Gen a = S.State StdGen a

            mkStdGen :: Int -> StdGen
            mkStdGen = Random.mkStdGen . (\* (3 :: Int) ^ (20 :: Int))

            -- random gen: implement a monad with the few functions that are necessary
            -- to generate samples, this monadic interface basically tells us the
            -- bare bones of what we need

            class Arb a where
            arb :: Int -> Gen a

            arb1 :: MonadGen m => Int -> m a

            class Eval a where
            evalVerifier :: RandomGen.Gen a -> Int -> a

            class Monad m => MonadGen m where
            oneOf :: [m a] -> m a
            generate :: m a -> IO a ```

* What parts of your project correspond to something that we talked about in class?
  - Overarching lesson: monads let us glue things together.
    - In our project, we broke a randomized algorithm into a straightforward deterministic algorithm and a monad that provides randomness
  - Modularize computation and don’t muddle monads
    - We separated any IO component from testing to reuse code as much as possible
  - Monads as interfaces
    - MonadGen acts as interface between an implementation that holds randomness and our project
    - In other classes, we might just load in a whole package and shop around for the methods that we want
    - Instead, this interface distills the bare bones of “randomness” that we need for these randomized algorithms
* Was there any part of your project that you had to scrap and redesign? What didn't work the first time? What was the hardest part to get correct? Why?

  - RandomGen

    - Before committing to MonadGen, we tried a complicated State model

      ````type Gen a = S.State StdGen a
          mkStdGen :: Int -> StdGen
          mkStdGen = Random.mkStdGen . (* (3 :: Int) ^ (20 :: Int))

          class Eval a where
          evalVerifier :: RandomGen.Gen a -> Int -> a

          class Monad m => MonadGen m where
          oneOf :: [m a] -> m a
          generate :: m a -> IO a

          -- implement class for gen, qc gen

          instance Eval Bool where
          evalVerifier g 0 = True
          evalVerifier g i | i > 0 = do
          let vals = S.evalState (replicateM i g) (mkStdGen 1)
          and vals
          evalVerifier g _ = False```
      ````

    * The State attempt was clumsy and didn’t allow for multiple instances

      - Difficulty: articulating the rules to “play by” (i.e. defining the monadic interface)
      - Statistical Quick Check
        - Statistical quick check module was difficult to get right because there were so many options.
        - Debated between a state based tracker that tracks the performance of every test case– decided this was redundant.

      ```
      type Eval = S.State (Int, Int)

      evalTest :: Eval ()
      evalTest = do
       (numTests, numFailure) <- S.get
       let b = runTest
       if b
         then S.put (numTests + 1, numFailure)
         else S.put (numTests + 1, numFailure + 1)

      runTest :: Bool
      runTest = undefined

      -- given a property, run k samples on it and return the error
      ```

      - Tried implementing a class that had a function determining approximate correctness with some input parameters but decided this was not verbose since SAT and matrix had different numbers of inputs/meanings. The correct value of approximation and verification algorithms are different.

      ```
      display :: (MonadGen m) => m [Bool] -> IO ()
      display gen = do
       x <- generate gen
       let len = length x
       let correct = foldr (\b acc -> if b then acc + 1 else acc) 0 x
       putStrLn ("Accuracy: " ++ show correct ++ "/" ++ show len)

      data Args = Args
       { twosidederror :: Bool,
         maxSuccess :: Int,
         epsilon :: Int,
         delta :: Int,
         maxDiscardedRatio :: Int
       }
       deriving (Show, Read)

      type Property = S.State Bool

      class SC where
       --   statCheck :: StateT (Property ()) (RandomGen.Gen a) Bool
       statCheck :: RandomGen.Gen a -> Property () -> Bool
       statCheck g p = undefined
       (~=~) :: a -> RandomGen.Gen [a] -> Bool
       -- computes weather something is approximatley correct
       (~=~) = undefined
      ```

- What sort of testing did you do to verify the correctness of your code? (Unit tests, quickcheck properties, etc.)
  - Unit tests for all deterministic operations (Matrix and SAT)
  - Making sure that our valuations assign a Bool to each var in the CNF
  - Implemented statistical QC to evaluate randomized functions
- What is something you would like your program to do but it doesn't do yet? How would you do these extensions?
  - The performance guarantees of randomized algorithms are over the probability distribution of its inner randomness, NOT the distribution of inputs
  - Ideally, if our randomized algorithm failed one example, our testing infrastructure could return this test
  - QuickCheck already does this; we could also use state to track what example we are considering and if an example fails, to store it in a state containing all failed examples
  - Other randomized algorithms such as streaming or communication algorithms
  - A complexity evaluator
- If you reimplemented this project in another language, what language would you choose? What would be easier? What would be more difficult?
  - Python: I’ve used python for randomized algorithms/monte carlo integration
    - - :All the existing packages make it quick and easy to compute an answer
    - 0: I’d implement each method with a bit of randomness/sampling inside it
    - -: it feels ad hoc and doesn’t generalize well, muddles the randomness and algorithm implementation
  - OCaml: Would be good to replicate the monadic aspect, which is what is really cool in our project
- What did you learn from this project?
  - Randomized algorithms as distributions of deterministic algorithms
    - This was a lesson from CIS 677 but is codified by monad design
    - Once you define a deterministic function f :: a -> Bool, then inject the randomness with a gen :: MonadGen m => m a, just using fmap f gen completes the task
    - Implementation in Haskell can deepen the “right” way to think about randomized algorithms
  - Monads force us to consider what is absolutely necessary
    - Working with quickcheck is almost too nice as there’s so many features
    - To implement probabilistic check, we don’t really need all that
    - Monads help filter out only the essence of randomness that our particular project needs to succeed
