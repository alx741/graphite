module Data.Graph.UGraphSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Graph.UGraph
import Data.Graph.Types

spec :: Spec
spec = do
    describe "Undirected Graph (UGraph)" $ do
        it "Can tell if a vertex exists" $ property $ do
            let g = insertVertex 1 empty :: UGraph Int ()
            let g' = insertVertex 2 empty :: UGraph Int ()
            containsVertex g 1 `shouldBe` True
            containsVertex g' 1 `shouldBe` False

        it "Can tell if an edge exists" $ property $ do
            let g = insertEdge (1 <-> 2) empty :: UGraph Int ()
            containsEdge g (1 <-> 2) `shouldBe` True

        it "Stores symetrical edges" $ property $ do
            let g = insertEdge (2 <-> 1) $ insertEdge (1 <-> 2) empty :: UGraph Int ()
            containsEdge g (1 <-> 2) `shouldBe` True
            containsEdge g (2 <-> 1) `shouldBe` True
            length (edges g) `shouldBe` 1

        it "Increments its order when a new vertex is inserted" $ property $
            \g v -> (not $ g `containsVertex` v)
                ==> order g + 1 == order (insertVertex v (g :: UGraph Int ()))
        it "Increments its size when a new edge is inserted" $ property $
            \g e -> (not $ g `containsEdge` e)
                ==> size g + 1 == size (insertEdge e (g :: UGraph Int ()))

        it "order is conserved" $ property $
            \g v -> (not $ g `containsVertex` v)
                ==> order g == order (removeVertex v $ insertVertex v (g :: UGraph Int ()))
        it "size is conserved" $ property $
            \g e -> (not $ g `containsEdge` e)
                ==> size g == size (removeEdge e $ insertEdge e (g :: UGraph Int ()))

        it "Is id when inserting and removing a new vertex" $ property $
            \g v -> (not $ g `containsVertex` v)
                ==> (removeVertex v $ insertVertex v g)
                    == (g :: UGraph Int ())
