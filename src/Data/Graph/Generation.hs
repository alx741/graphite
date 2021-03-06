{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Generation
    (
    -- * Erdős–Rényi model
      erdosRenyi
    , erdosRenyiU
    , erdosRenyiD

    -- * General Random graphs
    , rndGraph
    , rndGraph'

    -- * Random adjacency matrix
    , rndAdjacencyMatrix
    ) where

import Control.Monad (replicateM)
import Data.List     (foldl')
import System.Random

import Data.Hashable

import Data.Graph.DGraph
import Data.Graph.Types
import Data.Graph.UGraph

-- | Generate a random Erdős–Rényi G(n, p) model graph
erdosRenyi :: Graph g => Int -> Float -> IO (g Int ())
erdosRenyi n p = rndGraph' p [1..n]

-- | 'erdosRenyi' convinience 'UGraph' generation function
erdosRenyiU :: Int -> Float -> IO (UGraph Int ())
erdosRenyiU = erdosRenyi

-- | 'erdosRenyi' convinience 'DGraph' generation function
erdosRenyiD :: Int -> Float -> IO (DGraph Int ())
erdosRenyiD = erdosRenyi


-- | Generate a random graph for all the vertices of type /v/ in the list,
-- random edge attributes in /e/ within given bounds, and some existing
-- probability for each possible edge as per the Erdős–Rényi model
rndGraph :: forall g v e . (Graph g, Hashable v, Eq v, Random e)
 => (e, e)
 -> Float
 -> [v]
 -> IO (g v e)
rndGraph edgeBounds p verts = go verts (probability p) empty
    where
        go :: [v] -> Float -> g v e -> IO (g v e)
        go [] _ g = return g
        go (v:vs) pv g = do
            rnds <- replicateM (length vs + 1) $ randomRIO (0.0, 1.0)
            flipDir <- randomRIO (True, False)
            edgeAttr <- randomRIO edgeBounds
            let vs' = zip rnds vs
            let g' = insertVertex v g
            go vs pv $! foldl' (insertFlippedEdge pv v edgeAttr flipDir) g' vs'


-- | Same as 'rndGraph' but uses attributeless edges
rndGraph' :: forall g v . (Graph g, Hashable v, Eq v)
 => Float
 -> [v]
 -> IO (g v ())
rndGraph' p verts = go verts (probability p) empty
    where
        go :: [v] -> Float -> g v () -> IO (g v ())
        go [] _ g = return g
        go (v:vs) pv g = do
            rnds <- replicateM (length vs + 1) $ randomRIO (0.0, 1.0)
            flipDir <- randomRIO (True, False)
            let vs' = zip rnds vs
            let g' = insertVertex v g
            go vs pv $! foldl' (insertFlippedEdge pv v () flipDir) g' vs'


-- | Generate a random adjacency matrix
--
-- Useful for use with 'fromAdjacencyMatrix'
rndAdjacencyMatrix :: Int -> IO [[Int]]
rndAdjacencyMatrix n = replicateM n randRow
    where randRow = replicateM n (randomRIO (0,1)) :: IO [Int]

-- | Insert and edge between vertices if the probability is met
insertFlippedEdge :: (Graph g, Hashable v, Eq v)
 => Float
 -> v
 -> e
 -> Bool
 -> g v e
 -> (Float, v)
 -> g v e
insertFlippedEdge pv v edgeAttr flipDir g (p', v')
    | p' < pv = insertEdgeTriple triple g
    | otherwise = g
        where triple = if flipDir then (v', v, edgeAttr) else (v, v', edgeAttr)

-- | Bound a real value as probability value [0.0, 1.0]
probability :: Float -> Float
probability v | v >= 1 = 1 | v <= 0 = 0 | otherwise = v
