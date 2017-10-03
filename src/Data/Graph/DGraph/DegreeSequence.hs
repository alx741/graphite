module Data.Graph.DGraphDegreeSequence where

-- | The Degree Sequence of a 'DGraph' is a list of pairs (Indegree, Outdegree)
newtype DegreeSequence = DegreeSequence { unDegreeSequence :: [(Int, Int)] }
    deriving (Eq, Ord, Show)
