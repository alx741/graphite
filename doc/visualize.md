# Visualizing graphs

Graphite provides separate functions for visualizing undirected and directed
graphs: `plotUGraph` and `plotDGraph`.

Lets define a directed graph:

```Haswell
foundationUniverse :: DGraph String ()
foundationUniverse = fromArcsList
    [ "Helicon" --> "Nishaya"
    , "Helicon" --> "Wencory"
    , "Nishaya" --> "Wencory"
    , "Wencory" --> "Getorin"
    , "Wencory" --> "Cinna"
    , "Wencory" --> "Lystena"
    , "Lystena" --> "Helicon"
    , "Cinna"   --> "Florina"
    , "Florina" --> "Nishaya"
    , "Florina" --> "Derowd"
    , "Derowd"  --> "Cinna"
    , "Derowd"  --> "Lystena"
    ]
```

And visualize it with `plotDGraph`:

```haskell
main :: IO ()
main = plotDGraph foundationUniverse
```

![Foundation Universe (Directed)](./graphs/dfoundation.png)

We could now convert this graph to an undirected one using `toUndirected` and
then visualize it with `plotUGraph`:

```haskell
main :: IO ()
main = plotUGraph $ toUndirected foundationUniverse
```

![Foundation Universe (Undirected)](./graphs/ufoundation.png)



## Visualizing edged graphs


## Plotting to PNG images
