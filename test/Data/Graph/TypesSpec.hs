module Data.Graph.TypesSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Graph.Types

spec :: Spec
spec = do
    describe "Undirected Edge" $ do
        it "Satisfies equality for two vertices regardless of their order" $ property $
            \(v1, v2) -> (v1 <-> v2) == ((v2 <-> v1) :: Edge Int ())

    describe "Directed Arc" $ do
        it "Satisfies equality for two vertices when order is preserved" $ property $
            \(v1, v2) -> (v1 --> v2) == ((v1 --> v2) :: Arc Int ())
        it "Fails equality for two vertices when order reversed" $ property $
            prop_ArcInequality


prop_ArcInequality :: Int -> Bool
prop_ArcInequality v = (v1 --> v2) /= ((v2 --> v1) :: Arc Int ())
    where
        v1 = v
        v2 = v+1
