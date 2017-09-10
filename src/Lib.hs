{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           Data.Hashable
import qualified Data.HashMap.Lazy as HM

data Edge v a
    = Edge v v a
    | WeightedEdge Double v v a

data Arc v a
    = Arc v v a
    | WeightedArc Double v v a

type DiGraph v a = HM.HashMap v (HM.HashMap v a)

empty :: (Hashable v) => DiGraph v (Arc v ())
empty = HM.empty

-- insertVertex :: (Vertex v, Edge e) => v -> DiGraph v e -> DiGraph v e
-- insertVertex v = HM.insert v HM.empty

-- insertArc :: (Vertex v, Arc a) => a -> DiGraph v e -> DiGraph v e
-- insertArc a g =
--     if HM.member from g
--     then undefined
--     else g
--     where
--         from = fromVertex a
--         to = toVertex a


-- -- mymap :: HM.HashMap Int String
-- -- mymap = HM.insert 1 "hola" HM.empty
