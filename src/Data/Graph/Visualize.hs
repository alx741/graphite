module Data.Graph.Visualize
    ( plotUndirectedIO
    , plotUndirectedXdgIO

    , plotDirectedIO
    , plotDirectedXdgIO
    ) where

import Data.GraphViz
import Data.Hashable
import Data.Monoid    ((<>))
import System.Process

import Data.Graph.DGraph
import Data.Graph.Types
import Data.Graph.UGraph

-- | Plot an undirected 'UGraph' to a PNG image file
plotUndirectedIO :: (Show e) => UGraph Int e -> FilePath -> IO FilePath
plotUndirectedIO g fp = addExtension (runGraphvizCommand Sfdp $ toUndirectedDot g) Png fp

-- | Same as 'plotUndirectedIO' but open the resulting image with /xdg-open/
plotUndirectedXdgIO :: (Show e) => UGraph Int e -> FilePath -> IO ()
plotUndirectedXdgIO g fp = do
    fp' <- plotUndirectedIO g fp
    _ <- system $ "xdg-open " <> fp'
    return ()

-- | Plot a directed 'DGraph' to a PNG image file
plotDirectedIO :: (Show e) => DGraph Int e -> FilePath -> IO FilePath
plotDirectedIO g fp = addExtension (runGraphvizCommand Sfdp $ toDirectedDot g) Png fp

-- | Same as 'plotDirectedIO' but open the resulting image with /xdg-open/
plotDirectedXdgIO :: (Show e) => DGraph Int e -> FilePath -> IO ()
plotDirectedXdgIO g fp = do
    fp' <- plotDirectedIO g fp
    _ <- system $ "xdg-open " <> fp'
    return ()

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
