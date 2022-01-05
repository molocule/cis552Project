import Matrix
--(Vector, tRunRandomVerifierTest)
import qualified RandomGen
import Sat
--(CNF (..), Valuation, Var (..), satisfied, tRunRandomVerifierSat)
import Test.HUnit (Test (..), assertBool, runTestTT, (~:), (~?=))
import Test.QuickCheck ()

main :: IO ()
main = do
  putStrLn "Running Matrix Test Suite"
  runMatrixTests
  runSatTests

-- SAT Testing --
-- A few variables for test cases
vA, vB, vC, vD :: Var
vA = Var 'A'
vB = Var 'B'
vC = Var 'C'
vD = Var 'D'

exampleFormula :: CNF
exampleFormula =
  Conj
    [ Disj [Lit True vA, Lit True vB, Lit True vC],
      Disj [Lit False vA],
      Disj [Lit False vB, Lit True vC]
    ]

defaultNumVariables :: Int
defaultNumVariables = 7

testVars :: Test
testVars =
  "vars"
    ~: TestList
      [ vars exampleFormula ~?= [vA, vB, vC],
        vars (Conj []) ~?= [],
        vars (Conj [Disj []]) ~?= [],
        vars (Conj [Disj [Lit True vA]]) ~?= [vA],
        vars (Conj [Disj [Lit True vA, Lit False vC], Disj [Lit True vB]]) ~?= [vA, vB, vC]
      ]

testSatisfiedBy :: Test
testSatisfiedBy =
  "satisfiedBy"
    ~: TestList
      [ "not satisfied"
          ~: Disj [Lit True vA, Lit True vB, Lit True vC] `satisfied` Sat.fromList [(vA, False), (vB, False), (vC, False)] ~?= False,
        "repeat lit"
          ~: Disj [Lit True vA, Lit False vA, Lit True vB] `satisfied` Sat.fromList [(vA, False), (vB, False), (vC, False)] ~?= True
      ]

testAlgoN :: Test
testAlgoN =
  "algoN" ~: do
    c <- RandomGen.generate ((RandomGen.arb :: Int -> IO CNF) 10)
    v <- RandomGen.generate ((algoN :: CNF -> Int -> IO Valuation) c 1)
    return (vars c == Sat.keys v)

runSatTests :: IO ()
runSatTests = do
  _ <-
    runTestTT $
      TestList
        [ testVars,
          testSatisfiedBy,
          testAlgoN
        ]
  -- tRunRandomVerifierSat
  putStrLn "all done with SAT"

{-
MATRIX TESTING
--------------
Test Suite for matrix verification
-}

vector1 :: Vector
vector1 = V [1, 2, 3]

vector2 :: Vector
vector2 = V [3, 4, 5]

vector3 :: Vector
vector3 = V [1, 2]

vector4 :: Vector
vector4 = V [1, 3]

vector5 :: Vector
vector5 = V [2, 4]

vector6 :: Vector
vector6 = V [2, 5]

vector7 :: Vector
vector7 = V [1, 4]

vector8 :: Vector
vector8 = V [4, 5]

vector9 :: Vector
vector9 = V [3, 5]

{-
Testing for Matrix Operations
-}

tVectorLen :: Test
tVectorLen =
  "Vector Length"
    ~: TestList
      [ vectorLen vector1 ~?= 3,
        vectorLen (V []) ~?= 0,
        vectorLen vector3 ~?= 2
      ]

tMatDim :: Test
tMatDim =
  "Matrix Dimension"
    ~: TestList
      [ matDim (M [vector1, vector2]) ~?= (2, 3),
        matDim (M [vector3, vector4]) ~?= (2, 2),
        matDim (M []) ~?= (0, 0)
      ]

tIsSquare :: Test
tIsSquare =
  "Matrix Square"
    ~: TestList
      [ isSquare (M [vector1, vector2]) ~?= False,
        isSquare (M [vector3, vector4]) ~?= True,
        isSquare (M []) ~?= True
      ]

tDimCompat :: Test
tDimCompat =
  "Matrix Dimension Compatibility"
    ~: TestList
      [ dimCompat (M [vector1]) (M [vector3]) ~?= False,
        dimCompat (M [vector1, vector2]) (M [vector3, vector4]) ~?= False,
        dimCompat (M [vector3, vector4]) (M [vector1, vector2]) ~?= True,
        dimCompat (M []) (M []) ~?= True,
        dimCompat (M []) (M [V [1]]) ~?= False,
        dimCompat (M [vector1]) (transpose (M [vector1])) ~?= True
      ]

tVectorMatCompat :: Test
tVectorMatCompat =
  "Matrix Vector Compatibility"
    ~: TestList
      [ vectorMatCompat (M [vector1]) vector2 ~?= True,
        vectorMatCompat (M [vector1]) vector3 ~?= False,
        vectorMatCompat (M [vector1]) vector3 ~?= False,
        vectorMatCompat (M [vector1, vector2]) vector3 ~?= False,
        vectorMatCompat (M []) (V []) ~?= True,
        vectorMatCompat (M []) (V [1]) ~?= False
      ]

tMatCompat :: Test
tMatCompat =
  "Matrix Compatibility"
    ~: TestList
      [ matCompat (M [vector1]) (M [vector3]) ~?= False,
        matCompat (M [vector1, vector2]) (M [vector3, vector4]) ~?= False,
        matCompat (M [vector5, vector6]) (M [vector3, vector4]) ~?= True,
        matCompat (M []) (M []) ~?= True,
        matCompat (M []) (M [V [1]]) ~?= False,
        matCompat (M [vector1]) (transpose (M [vector1])) ~?= False
      ]

tDot :: Test
tDot =
  "Dot Product"
    ~: TestList
      [ dot vector1 vector2 ~?= 26,
        dot vector3 vector4 ~?= 7,
        dot (V []) (V []) ~?= 0,
        dot (V []) (V [1]) ~?= 0
      ]

tTransposeVector :: Test
tTransposeVector =
  "transposeVector"
    ~: TestList
      [ transposeVector (V []) ~?= M [],
        transposeVector (V [1, 2, 3]) ~?= M [V [1], V [2], V [3]]
      ]

ttranspose :: Test
ttranspose =
  "transpose"
    ~: TestList
      [ transpose (M [vector1, vector2]) ~?= M [vector4, vector5, vector9],
        transpose (M [vector1, vector8]) ~?= M [vector7, vector6],
        transpose (M [vector2]) ~?= M [V [3], V [4], V [5]],
        transpose (M [V [1], V []]) ~?= M [],
        transpose (M [V [], V [1]]) ~?= M [],
        transpose (M []) ~?= M [],
        transpose (M [V [1, 2, 3], V [4, 5, 6]]) ~?= M [V [1, 4], V [2, 5], V [3, 6]]
      ]

tMatrixMult :: Test
tMatrixMult =
  "Matrix Eval"
    ~: TestList
      [ matrixMult (M [vector1, vector2]) (M [vector3, vector5, vector8]) ~?= M [V [17, 25], V [31, 47]],
        matrixMult (M [vector3, vector4]) (M [vector5, vector6]) ~?= M [V [6, 14], V [8, 19]],
        matrixMult (M []) (M []) ~?= M []
      ]

runMatrixTests :: IO ()
runMatrixTests = do
  _ <-
    runTestTT $
      TestList
        [ tVectorLen,
          tMatDim,
          tIsSquare,
          tDimCompat,
          tVectorMatCompat,
          tMatCompat,
          tDot,
          tTransposeVector,
          ttranspose,
          tMatrixMult
        ]
  putStrLn "all done with Matrix"

-- tRunRandomVerifierTest
