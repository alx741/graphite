{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Generation where

import Control.Monad (replicateM)
import Data.List     (foldl')
import System.Random

import Data.Hashable

import Data.Graph.DGraph
import Data.Graph.Types
import Data.Graph.UGraph

-- | Generate a random Erdős–Rényi G(n, p) model graph of vertices within given
-- bounds, with a connection probability
erdosRenyi :: (Graph g, Hashable v, Eq v, Enum v) => (v, v) -> Float -> IO (g v ())
erdosRenyi (n1, n2) p = go [n1..n2] (probability p) empty
    where
        -- go :: Graph g => [Int] -> Float -> g Int () -> IO (g Int ())
        go [] _ g = return g
        go (v:vs) pv g = do
            rnds <- replicateM (length vs + 1) $ randomRIO (0.0, 1.0)
            flipDir <- randomRIO (True, False)
            let vs' = zip rnds vs
            let g' = insertVertex v g
            go vs pv $! foldl' (putV pv v flipDir) g' vs'

        -- putV :: Graph g => Float -> Int -> Bool -> g Int () -> (Float, Int) -> g Int ()
        putV pv v flipDir g (p', v')
            | p' < pv = insertEdgePair pair g
            | otherwise = g
                where pair = if flipDir then (v', v) else (v, v')

        -- probability :: Float -> Float
        probability v | v >= 1 = 1 | v <= 0 = 0 | otherwise = v

-- | 'erdosRenyi' convinience 'UGraph' generation function
erdosRenyiU :: Int -> Float -> IO (UGraph Int ())
erdosRenyiU n = erdosRenyi (1, n)

-- | 'erdosRenyi' convinience 'DGraph' generation function
erdosRenyiD :: Int -> Float -> IO (DGraph Int ())
erdosRenyiD n = erdosRenyi (1, n)

-- | Same as 'erdosRenyi' but generates edges with random numerical attributes
-- as weights, within given bounds
-- erdosRenyiWeighted :: (Graph g, Num a) => Int -> Float -> (a, a) -> IO (g Int a)
-- erdosRenyiWeighted n p = go [1..n] (probability p) empty
--     where
--         -- go :: Graph g => [Int] -> Float -> g Int () -> IO (g Int ())
--         go [] _ g = return g
--         go (v:vs) pv g = do
--             rnds <- replicateM (length vs + 1) $ randomRIO (0.0, 1.0)
--             flipDir <- randomRIO (True, False)
--             let vs' = zip rnds vs
--             let g' = insertVertex v g
--             go vs pv $! foldl' (putV pv v flipDir) g' vs'

--         putV :: Graph g => Float -> Int -> Bool -> g Int () -> (Float, Int) -> g Int ()
--         putV pv v flipDir g (p', v')
--             | p' < pv = insertEdgePair pair g
--             | otherwise = g
--                 where pair = if flipDir then (v', v) else (v, v')

--         probability :: Float -> Float
--         probability v | v >= 1 = 1 | v <= 0 = 0 | otherwise = v

-- | Generate a random square binary matrix
-- | Useful for use with 'fromAdjacencyMatrix'
randomMat :: Int -> IO [[Int]]
randomMat n = replicateM n randRow
    where randRow = replicateM n (randomRIO (0,1)) :: IO [Int]
