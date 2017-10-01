{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Generation where

import Control.Monad (replicateM)
import Data.List     (foldl')
import System.Random

import Data.Graph.Types

-- | Probability value between 0 and 1
newtype Probability = P Double deriving (Eq, Ord, Show)

-- | Construct a 'Probability' value
probability :: Double -> Probability
probability v | v >= 1 = P 1 | v <= 0 = P 0 | otherwise = P v

-- | Generate a random Erdős–Rényi  G(n, p) model graph
erdosRenyi :: Graph g => Int -> Probability -> IO (g Int ())
erdosRenyi n (P p) = go [1..n] p empty
    where
        go :: Graph g => [Int] -> Double -> g Int () -> IO (g Int ())
        go [] _ g = return g
        go (v:vs) pv g = do
            rnds <- replicateM (length vs + 1) $ randomRIO (0.0, 1.0)
            flipDir <- randomRIO (True, False)
            let vs' = zip rnds vs
            let g' = insertVertex g v
            go vs pv $! (foldl' (putV pv v flipDir) g' vs')

        putV :: Graph g => Double -> Int -> Bool -> g Int () -> (Double, Int) -> g Int ()
        putV pv v flipDir g (p', v')
            | p' < pv = insertEdgePair g pair
            | otherwise = g
                where pair = if flipDir then (v', v) else (v, v')

-- | Generate a random square binary matrix
-- | Useful for use with 'fromAdjacencyMatrix'
randomMat :: Int -> IO [[Int]]
randomMat n = replicateM n randRow
    where randRow = replicateM n (randomRIO (0,1)) :: IO [Int]
