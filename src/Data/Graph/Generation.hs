{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Generation where

import Control.Monad (replicateM)
import Data.List     (foldl')
import System.Random

import Data.Graph.DGraph
import Data.Graph.Types
import Data.Graph.UGraph

-- | Generate a random Erdős–Rényi G(n, p) model graph of /n/ vertices with a
-- | /p/ connection probability
erdosRenyi :: Graph g => Int -> Float -> IO (g Int ())
erdosRenyi n p = go [1..n] (probability p) empty
    where
        go :: Graph g => [Int] -> Float -> g Int () -> IO (g Int ())
        go [] _ g = return g
        go (v:vs) pv g = do
            rnds <- replicateM (length vs + 1) $ randomRIO (0.0, 1.0)
            flipDir <- randomRIO (True, False)
            let vs' = zip rnds vs
            let g' = insertVertex g v
            go vs pv $! (foldl' (putV pv v flipDir) g' vs')

        putV :: Graph g => Float -> Int -> Bool -> g Int () -> (Float, Int) -> g Int ()
        putV pv v flipDir g (p', v')
            | p' < pv = insertEdgePair g pair
            | otherwise = g
                where pair = if flipDir then (v', v) else (v, v')

        probability :: Float -> Float
        probability v | v >= 1 = 1 | v <= 0 = 0 | otherwise = v

-- | 'erdosRenyi' convinience 'UGraph' generation function
erdosRenyiU :: Int -> Float -> IO (UGraph Int ())
erdosRenyiU  = erdosRenyi

-- | 'erdosRenyi' convinience 'DGraph' generation function
erdosRenyiD :: Int -> Float -> IO (DGraph Int ())
erdosRenyiD  = erdosRenyi

-- | Generate a random square binary matrix
-- | Useful for use with 'fromAdjacencyMatrix'
randomMat :: Int -> IO [[Int]]
randomMat n = replicateM n randRow
    where randRow = replicateM n (randomRIO (0,1)) :: IO [Int]
