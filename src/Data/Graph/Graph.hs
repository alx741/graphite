module Data.Graph.Graph where

import qualified Data.HashMap.Lazy as HM

import Data.Graph.Types

-- | Undirected Graph of Vertices in /v/ and Edges with attributes in /e/
type Graph v e = HM.HashMap v (Links v e)
