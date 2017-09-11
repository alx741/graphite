module Data.Graph.DGraphSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Graph.DGraph

spec :: Spec
spec = do
    describe "Directed Graph (DGraph)" $ do
        it "Increments its order when a new vertex is inserted" $ property $
            \g v -> (not $ g `containsVertex` v) ==> order g + 1 == order (insertVertex v (g :: DGraph Int ()))
        it "Remains untouched when inserting an existing vertex" $ property $
            \g v -> (g `containsVertex` v) ==> g == (insertVertex v (g :: DGraph Int ()))
