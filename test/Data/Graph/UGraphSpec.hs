module Data.Graph.UGraphSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Graph.Types
import Data.Graph.UGraph

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

        it "Its order corresponds to the number of vertices" $ property $
            \g -> order g == length (vertices (g :: UGraph Int ()))

        it "Its size corresponds to the number of edges" $ property $
            \g -> size g == length (edges (g :: UGraph Int ()))

        it "Increments its order when a new vertex is inserted" $ property $
            \g v -> (not $ g `containsVertex` v)
                ==> order g + 1 == order (insertVertex v (g :: UGraph Int ()))

        it "Increments its size when a new edge is inserted" $ property $
            \g e -> (not $ g `containsEdge` e)
                ==> size g + 1 == size (insertEdge e (g :: UGraph Int ()))

        it "Increments its order only for new vertices" $ property $
            \v -> order (insertVertex v $ insertVertex v (empty :: UGraph Int ())) == 1

        it "Increments its size only for new edges" $ property $
            \e -> size (insertEdge e $ insertEdge e (empty :: UGraph Int ())) == 1

        it "Decrements its order only when existing vertices are removed" $ property $
            \v1 v2 -> (v1 /= v2)
                ==> order (removeVertex v2 $ insertVertex v1 (empty :: UGraph Int ())) == 1

        it "Decrements its size only when existing edges are removed" $ property $
            \e1 e2 -> (e1 /= e2)
                ==> size (removeEdge e2 $ insertEdge e1 (empty :: UGraph Int ())) == 1

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

        it "Can be represented as a list" $ property $
            \g -> (fromList . toList) g == (g :: UGraph Int ())
