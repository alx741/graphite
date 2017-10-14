module Data.Graph.Traversal
    ( bfsVertices
    , dfsVertices
    ) where

import Data.List (foldl')

import           Data.Hashable
import qualified Data.Set      as S

import Data.Graph.Types

-- | breadh-first-search vertices starting at a particular vertex
bfsVertices :: (Graph g, Hashable v, Eq v, Ord v) => g v e -> v -> [v]
bfsVertices g fromV = go [fromV] S.empty []
    where
        go [] _ popped = popped
        go (v:vs) visited popped =
            let reachables = nonVisitedReachables g visited v
            in go
                (vs ++ reachables)
                (setInsertMany visited $ v : reachables)
                (popped ++ [v])

-- | depth-first-search vertices starting at a particular vertex
dfsVertices :: (Graph g, Hashable v, Eq v, Ord v) => g v e -> v -> [v]
dfsVertices g fromV = go [fromV] S.empty []
    where
        go [] _ popped = popped
        go (v:vs) visited popped =
            let reachables = nonVisitedReachables g visited v
            in go
                (reachables ++ vs)
                (setInsertMany visited $ v : reachables)
                (popped ++ [v])

nonVisitedReachables :: (Graph g, Hashable v, Eq v, Ord v)
 => g v e
 -> S.Set v
 -> v
 -> [v]
nonVisitedReachables g visited v = filter
    (\v' -> not $ S.member v' visited)
    (reachableAdjacentVertices g v)

setInsertMany :: Ord a => S.Set a -> [a] -> S.Set a
setInsertMany = foldl' (flip S.insert)
