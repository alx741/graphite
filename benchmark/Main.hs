module Main where

import Criterion.Main

import Control.DeepSeq
import Data.Graph.Generation

import Data.Graph.DGraph
import Data.Graph.Types
import Data.Graph.UGraph

main = do
    ug100 <- force <$> erdosRenyi 100 0.3 :: IO (UGraph Int ())
    ug500 <- force <$> erdosRenyi 500 0.3 :: IO (UGraph Int ())
    ug1000 <- force <$> erdosRenyi 1000 0.3 :: IO (UGraph Int ())

    dg100 <- force <$> erdosRenyi 100 0.3 :: IO (DGraph Int ())
    dg500 <- force <$> erdosRenyi 500 0.3 :: IO (DGraph Int ())
    dg1000 <- force <$> erdosRenyi 1000 0.3 :: IO (DGraph Int ())

    defaultMain
        [ bgroup "generation"
            [ bench "gnp_100" $ nfIO (erdosRenyi 100 0.3 :: IO (UGraph Int ()))
            , bench "gnp_500" $ nfIO (erdosRenyi 500 0.3 :: IO (UGraph Int ()))
            , bench "gnp_1000" $ nfIO (erdosRenyi 1000 0.3 :: IO (UGraph Int ()))
            , bench "gnp_5000" $ nfIO (erdosRenyi 2000 0.3 :: IO (UGraph Int ()))
            ]

        , bgroup "edges"
            [ bench "edges_ug100" $ nf edges ug100
            , bench "edges_ug500" $ nf edges ug500
            , bench "edges_ug1000" $ nf edges ug1000

            , bench "arcs_dg100" $ nf arcs dg100
            , bench "arcs_dg500" $ nf arcs dg500
            , bench "arcs_dg1000" $ nf arcs dg1000
            ]

        , bgroup "properties"
            [ bench "order_ug100" $ nf order ug100
            , bench "size_ug100" $ nf size ug100
            , bench "order_ug500" $ nf order ug500
            , bench "size_ug500" $ nf size ug500
            , bench "order_ug1000" $ nf order ug500
            , bench "size_ug1000" $ nf size ug500

            , bench "order_dg100" $ nf order dg100
            , bench "size_dg100" $ nf size dg100
            , bench "order_dg500" $ nf order dg500
            , bench "size_dg500" $ nf size dg500
            , bench "order_dg1000" $ nf order dg500
            , bench "size_dg1000" $ nf size dg500
            ]
        ]
