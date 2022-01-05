module Main where

import qualified Gen
import Matrix
-- (tErrorList, tRunFalseMatrixTest, tRunRandomVerifierCorrect, tRunStatCheckerQC, tRunTrueMatrixTest)
import qualified Matrix
import qualified RandomGen
import Sat
-- (tRunApproxChecker, tRunRandomVerifierSat)
import qualified Sat
import qualified State
import qualified StatisticalQC

{-
Main interface for interacting with the Randomized Algorithms Library
-}

matrixOptions :: String
matrixOptions =
  "Please enter one of the following choices: \n \
  \ 1 to see performance on given AB = C matrices, \n \
  \ 2 to see performence on given AB /= C matrices \n \
  \ 3 to see statistical quick check on random matrices \n \
  \ 4 to see our verifier on random AB = C matrices \n \
  \ 5 to see how error rate changes as matrix size ranges from 1x1 to 10x10"

satOptions :: String
satOptions =
  "Please select one of the following choices: \n \
  \ 1 to see a customizable SAT approximation, \n \
  \ 2 to see our correctness checker for SAT approximation"

invalid :: String
invalid = "Invalid Selection, please try again"

main :: IO ()
main = do
  putStrLn "Hello! Welcome to Randomized Algorithms implemented in Haskell"
  innerLoop
  where
    innerLoop :: IO ()
    innerLoop = do
      putStrLn "Please enter 1 to see randomized Matrix module and 2 to see SAT"
      a <- getLine
      let ans = (read a :: Int)
      case ans of
        1 -> do
          putStrLn matrixOptions
          b <- getLine
          let bns = (read b :: Int)
          case bns of
            1 -> do
              tRunTrueMatrixTest
            2 -> do
              tRunFalseMatrixTest
            3 -> do
              tRunStatCheckerQC
            4 -> do
              tRunRandomVerifierCorrect
            5 -> do
              tErrorList
            _ -> do
              putStrLn invalid
          innerLoop
        2 -> do
          putStrLn satOptions
          b <- getLine
          let bns = (read b :: Int)
          case bns of
            1 -> do
              tRunRandomVerifierSat
            2 -> do
              tRunApproxChecker
            _ -> do
              putStrLn invalid
          innerLoop
        _ -> do
          putStrLn invalid
          innerLoop
