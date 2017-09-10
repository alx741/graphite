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

-- | Simple Edge with no attributes
type SimpleEdge v = Edge v ()

-- | Simple Arc with no attributes
type SimpleArc v = Arc v ()

-- | Construct a 'SimpleEdge' for two vertices
(<->) :: (Hashable v) => v -> v -> SimpleEdge v
(<->) v1 v2 = Edge v1 v2 ()

-- | Construct a 'SimpleArc' for two vertices
(-->) :: (Hashable v) => v -> v -> SimpleArc v
(-->) v1 v2 = Arc v1 v2 ()


type DiGraph v e = HM.HashMap v (HM.HashMap v e)

empty :: (Hashable v) => DiGraph v e
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
