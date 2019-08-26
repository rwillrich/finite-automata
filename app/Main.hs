module Main where

import DFA (DFA(..), testDFA)
import qualified DFA
import NFA (NFA(..), testNFA)
import qualified NFA

import Control.Monad (forM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data State
  = Q0 | Q1 | Q2 | Q3
  deriving (Eq, Ord, Show)

transitionTableDFA :: Map State (Map Char State)
transitionTableDFA = Map.fromList
  [ (Q0, Map.fromList
      [ ('a', Q1)
      , ('b', Q2)
      ]
    )
  , (Q1, Map.fromList
      [ ('a', Q3)
      , ('b', Q2)
      ]
    )
  , (Q2, Map.fromList
      [ ('a', Q1)
      , ('b', Q3)
      ]
    )
  , (Q3, Map.fromList
      [ ('a', Q3)
      , ('b', Q3)
      ]
    )
  ]

transitionTableNFA :: Map State (Map Char (Set State))
transitionTableNFA = Map.fromList
  [ (Q0, Map.fromList
      [ ('a', Set.fromList [Q0, Q1])
      , ('b', Set.fromList [Q0, Q2])
      ]
    )
  , (Q1, Map.fromList
      [ ('a', Set.fromList [Q3])
      ]
    )
  , (Q2, Map.fromList
      [ ('b', Set.fromList [Q3])
      ]
    )
  , (Q3, Map.fromList
      [ ('a', Set.fromList [Q3])
      , ('b', Set.fromList [Q3])
      ]
    )
  ]

main :: IO ()
main = do
  let inputs = ["aa", "bb", "aba", "abaaba", "babbab"]
  putStrLn "DFAs"
  let dfa = DFA transitionTableDFA Q0 (Set.fromList [Q3])
  print $ DFA.alphabet dfa
  print $ DFA.states dfa
  forM_ inputs $ \input ->
    print (input, testDFA dfa input)
  putStrLn "NFAs"
  let nfa = NFA transitionTableNFA Q0 (Set.fromList [Q3])
  print $ NFA.alphabet nfa
  print $ NFA.states nfa
  forM_ inputs $ \input ->
    print (input, testNFA nfa input)
