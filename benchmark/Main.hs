module Main where

import Criterion.Main

import Data.Graph.Generation
import Data.Graph.Types
import Data.Graph.UGraph

main = defaultMain
    [ bgroup "generation"
        [ bench "gnp_100" $ whnfIO (erdosRenyi 100 0.3 :: IO (UGraph Int ()))
        , bench "gnp_500" $ whnfIO (erdosRenyi 500 0.3 :: IO (UGraph Int ()))
        , bench "gnp_1000" $ whnfIO (erdosRenyi 1000 0.3 :: IO (UGraph Int ()))
        ]
    ]
