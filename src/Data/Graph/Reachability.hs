module Data.Graph.Reachability where

import Data.Hashable

import Data.Graph.Types

canReach :: (Graph g, Hashable v, Eq v) => g v e -> v -> v -> Bool
canReach g v1 v2 = containsEdgePair g (v1, v2)
