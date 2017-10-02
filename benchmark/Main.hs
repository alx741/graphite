module Main where

import Criterion.Main

import Data.Graph.Generation
import Data.Graph.Types
import Data.Graph.UGraph

main = do
    ug100 <- erdosRenyi 100 0.3 :: IO (UGraph Int ())
    ug500 <- erdosRenyi 500 0.3 :: IO (UGraph Int ())

    defaultMain
        [ bgroup "generation"
            [ bench "gnp_100" $ whnfIO (erdosRenyi 100 0.3 :: IO (UGraph Int ()))
            , bench "gnp_500" $ whnfIO (erdosRenyi 500 0.3 :: IO (UGraph Int ()))
            , bench "gnp_1000" $ whnfIO (erdosRenyi 1000 0.3 :: IO (UGraph Int ()))
            ]

        , bgroup "properties"
            [ bench "order_ug100" $ whnf order ug100
            , bench "size_ug100" $ whnf size ug100
            , bench "order_ug500" $ whnf order ug500
            , bench "size_ug500" $ whnf size ug500
            ]
        ]
