module Data.Graph.DGraphSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Graph.DGraph

spec :: Spec
spec = do
    describe "Directed Graph (DGraph)" $ do
        it "Increments its order when a new vertex is inserted" $ property $
            \g v -> (not $ g `containsVertex` v) ==> order g + 1 == order (insertVertex v (g :: DGraph Int ()))
        it "Increments its size when a new arc is inserted" $ property $
            \g arc -> (not $ g `containsArc` arc) ==> size g + 1 == size (insertArc arc (g :: DGraph Int ()))
