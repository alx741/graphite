module Main where

import Data.Graph.DGraph as DG
import Data.Graph.Graph  as G
import Data.Graph.Types
import Test.QuickCheck

main :: IO ()
main = do
    -- let g = insertEdge (1 <-> 2) (G.empty :: Graph Int ())
    g <- generate arbitrary :: IO (Graph Int ())
    -- let dg = insertArc (1 --> 2) (DG.empty :: DGraph Int ())
    print g
    print $ edges g
    -- print dg
