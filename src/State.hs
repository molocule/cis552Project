{-
---
Adapted from CIS 552: A Generic State Transformer
---

-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module State (State, get, put, modify, runState, evalState, execState) where

import Control.Monad (ap, liftM)

newtype State s a = S {runState :: s -> (a, s)}

instance Monad (State s) where
  return :: a -> State s a
  return x = S (x,) -- this tuple section (x,) is equivalent to \y -> (x,y)

  (>>=) :: State s a -> (a -> State s b) -> State s b
  st >>= f = S $ \s ->
    let (a, s') = runState st s
     in runState (f a) s'

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

evalState :: State s a -> s -> a
evalState st s = fst (runState st s)

execState :: State s a -> s -> s
execState st = snd . runState st

get :: State s s
get = S $ \s -> (s, s)

put :: s -> State s ()
put s' = S $ const ((), s')

modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put (f s)

{-
[1]: http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2
-}