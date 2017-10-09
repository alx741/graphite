module Data.Graph.DGraphSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Graph.DGraph
import Data.Graph.Types

spec :: Spec
spec = do
    describe "Directed Graph (DGraph)" $ do
        it "Can tell if a vertex exists" $ property $ do
            let g = insertVertex 1 empty :: DGraph Int ()
            let g' = insertVertex 2 empty :: DGraph Int ()
            containsVertex g 1 `shouldBe` True
            containsVertex g' 1 `shouldBe` False

        it "Can tell if an arc exists" $ property $ do
            let g = insertArc (1 --> 2) empty :: DGraph Int ()
            let g' = insertArc (2 --> 1) empty :: DGraph Int ()
            containsArc g (1 --> 2) `shouldBe` True
            containsArc g' (1 --> 2) `shouldBe` False

        it "Increments its order when a new vertex is inserted" $ property $
            \g v -> (not $ g `containsVertex` v)
                ==> order g + 1 == order (insertVertex v (g :: DGraph Int ()))
        it "Increments its size when a new arc is inserted" $ property $
            \g a -> (not $ g `containsArc` a)
                ==> size g + 1 == size (insertArc a (g :: DGraph Int ()))

        it "order is conserved" $ property $
            \g v -> (not $ g `containsVertex` v)
                ==> order g == order (removeVertex v $ insertVertex v (g :: DGraph Int ()))
        it "size is conserved" $ property $
            \g a -> (not $ g `containsArc` a)
                ==> size g == size (removeArc a $ insertArc a (g :: DGraph Int ()))

        it "Is id when inserting and removing a new vertex" $ property $
            \g v -> (not $ g `containsVertex` v)
                ==> (removeVertex v $ insertVertex v g)
                    == (g :: DGraph Int ())
