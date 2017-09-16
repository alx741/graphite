module Data.Graph.Visualize
    ( plotIO
    , plotXdgIO
    ) where

import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Hashable
import Data.Monoid                       ((<>))
import System.Process

import qualified Data.Graph.Graph as G
import           Data.Graph.Types

-- | Plot an undirected 'Graph' to a PNG image file
plotIO :: (Show e) => G.Graph Int e -> FilePath -> IO FilePath
plotIO g fp = addExtension (runGraphvizCommand Sfdp $ toDot' g) Png fp

-- | Same as 'plotIO' but open the resulting image with /xdg-open/
plotXdgIO :: (Show e) => G.Graph Int e -> FilePath -> IO ()
plotXdgIO g fp = do
    fp' <- plotIO g fp
    _ <- system $ "xdg-open " <> fp'
    return ()

labeledNodes :: (Show v) => G.Graph v e -> [(v, String)]
labeledNodes g = fmap (\v -> (v, show v)) $ vertices g

labeledEdges :: (Hashable v, Eq v, Show e) => G.Graph v e -> [(v, v, String)]
labeledEdges g = fmap (\(Edge v1 v2 attr) -> (v1, v2, show attr)) $ G.edges g

toDot' :: (Show e) => G.Graph Int e -> DotGraph Int
toDot' g = graphElemsToDot params (labeledNodes g) (labeledEdges g)
    where params = nonClusteredParams
            { isDirected = False
            , globalAttributes = [GraphAttrs
                [ NodeSep 1, Overlap ScaleOverlaps
                , Shape Circle
                ]]
            }
