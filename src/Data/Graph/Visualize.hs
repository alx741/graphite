module Data.Graph.Visualize
    ( plotUGraph
    , plotUGraphPng

    , plotDGraph
    , plotDGraphPng
    ) where

import Data.GraphViz
import Data.Hashable

import Data.Graph.DGraph
import Data.Graph.Types
import Data.Graph.UGraph

-- | Plot an undirected 'UGraph'
plotUGraph :: (Show e) => UGraph Int e -> IO ()
plotUGraph g = runGraphvizCanvas Sfdp (toUndirectedDot g) Xlib

-- | Plot an undirected 'UGraph' to a PNG image file
plotUGraphPng :: (Show e) => UGraph Int e -> FilePath -> IO FilePath
plotUGraphPng g fp = addExtension (runGraphvizCommand Sfdp $ toUndirectedDot g) Png fp

-- | Plot a directed 'DGraph'
plotDGraph :: (Show e) => DGraph Int e -> IO ()
plotDGraph g = runGraphvizCanvas Sfdp (toDirectedDot g) Xlib

-- | Plot a directed 'DGraph' to a PNG image file
plotDGraphPng :: (Show e) => DGraph Int e -> FilePath -> IO FilePath
plotDGraphPng g fp = addExtension (runGraphvizCommand Sfdp $ toDirectedDot g) Png fp

labeledNodes :: (Graph g, Show v) => g v e -> [(v, String)]
labeledNodes g = fmap (\v -> (v, show v)) $ vertices g

labeledEdges :: (Hashable v, Eq v, Show e) => UGraph v e -> [(v, v, String)]
labeledEdges g = fmap (\(Edge v1 v2 attr) -> (v1, v2, show attr)) $ edges g

labeledArcs :: (Hashable v, Eq v, Show e) => DGraph v e -> [(v, v, String)]
labeledArcs g = fmap (\(Arc v1 v2 attr) -> (v1, v2, show attr)) $ arcs g

toUndirectedDot :: (Show e) => UGraph Int e -> DotGraph Int
toUndirectedDot g = graphElemsToDot params (labeledNodes g) (labeledEdges g)
    where params = nonClusteredParams { isDirected = False }

toDirectedDot :: (Show e) => DGraph Int e -> DotGraph Int
toDirectedDot g = graphElemsToDot params (labeledNodes g) (labeledArcs g)
    where params = nonClusteredParams { isDirected = True }
