{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module NFA
  ( NFA(..)
  , alphabet
  , states
  , testNFA
  ) where

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

data NFA state input where
  NFA :: (Ord state, Ord input) =>
    { transitionTable :: Map state (Map input (Set state))
    , initialState :: state
    , finalStates :: Set state
    }
    -> NFA state input

alphabet :: NFA state input -> Set input
alphabet (NFA { transitionTable }) = Map.foldl' (\a b -> Set.union a (Map.keysSet b)) Set.empty transitionTable

states :: NFA state input -> Set state
states (NFA { transitionTable }) = Map.keysSet transitionTable

transition :: NFA state input -> state -> input -> Maybe (Set state)
transition (NFA { transitionTable }) state input =
  Map.lookup state transitionTable >>= Map.lookup input

extendedTransition :: (Ord state) => NFA state input -> Set state -> [input] -> Maybe (Set state)
extendedTransition dfa states [] = Just states
extendedTransition dfa states (x:xs) = extendedTransition dfa newStates xs
  where
    newStates = Set.unions $ map transitionSet (Set.toList states)
    transitionSet state = fromMaybe Set.empty (transition dfa state x)

isAccepting :: NFA state input -> Set state -> Bool
isAccepting (NFA { finalStates }) state = (not . Set.null) $ Set.intersection state finalStates

testNFA :: NFA state input -> [input] -> Bool
testNFA nfa@(NFA { initialState }) input =
  maybe False (isAccepting nfa) (extendedTransition nfa (Set.singleton initialState) input)
