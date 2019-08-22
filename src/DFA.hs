{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module DFA
  ( DFA(..)
  , transition
  , testDFA
  ) where

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data DFA state input where
  DFA :: (Ord state, Ord input) =>
    { transitionTable :: Map state (Map input state)
    , initialState :: state
    , finalStates :: Set state
    }
    -> DFA state input

transition :: DFA state input -> state -> input -> Maybe state
transition (DFA { transitionTable }) state input =
  Map.lookup state transitionTable >>= Map.lookup input

extendedTransition :: DFA state input -> state -> [input] -> Maybe state
extendedTransition dfa state input =
  foldM (transition dfa) state input

isAccepting :: DFA state input -> state -> Bool
isAccepting (DFA { finalStates }) state = Set.member state finalStates

-- scanlMaybe :: (b -> a -> Maybe b) -> b -> [a] -> [b]
-- scanlMaybe f q [] = [q]
-- scanlMaybe f q (x:xs) = q : maybe [] (\y -> scanlMaybe f y xs) (f q x)
--
-- execDFA :: DFA state input -> [input] -> [state]
-- execDFA dfa@(DFA { initialState }) input =
--   scanlMaybe (transition dfa) initialState input

testDFA :: DFA state input -> [input] -> Bool
testDFA dfa@(DFA { initialState }) input =
  maybe False (isAccepting dfa) (extendedTransition dfa initialState input)
