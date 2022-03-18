
## Hotspot Detection for Spatial Social (Non-planar) Networks

**SSNtools** is an R package that provides metrics for analyzing and
visualizing spatial social networks.

**SSNtools** is currently equipped with functions to calculate hotspots
(i.e., heat) of spatial social networks. Traditional GIS hotspot
detection methods (e.g. the Getis-Ord GI\* statistic or Ripley’s
K-function) only apply to point patterns, and yet, clustered nodes in
network may not be connected.

The goal for the current function of **SSNtools** is to detect the
number of non-planar edges and the network density of a subset of a
social network contained within a focal window. In another words, the
algorithms return hotspots where nodes are not only densely located but
also connected.

See paper [Spatial Social Networks (SSN) Hot Spot Detection: Scan
Methods for Non-planar Networks](https://arxiv.org/pdf/2011.07702.pdf)
for detailed methodology.

#### Installation

You can install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("friendlycities-gatech/SSNtools")
```

#### Example

This is a basic example which shows you how to use the main functions of
SSN hotspot detections in SSNtools. The package comes with an example
dataset, which is a spatial social network of Mafia members in New York
City in the 1960s. See [To racketeer among neighbors: spatial features
of criminal collaboration in the American
Mafia](https://drive.google.com/file/d/1guVURnryYUyXaJ3A7SoMFMpkv7CUx6He/view)
for more details about the dataset. You can call **NYCMafiaNodes**
(n=298) and **NYCMafiaEdges**(n=946) to directly access the sample
dataset. The coordinate unit of the sample dataset is **meter**
(crs=32118). The function currently only applies to **undirected**
graph.

All the main functions (e.g, edgeScanRadius and NDScanRadius) will
return a list of two dataframes. The first dataframe is a node table
that has the node label and the **heat**, presenting the number of edges
in EdgeScan and network density in NDScan respectively. The second
dataframe is an edge table that has the edge pair and a binary column
**edgeWithin** indicating whether the edge is within the scanning
window.

``` r
library(SSNtools)

data(NYCMafiaNodes)
data(NYCMafiaEdges)

# ----process dataframe into a list of lists 
# params:
#     nodes - a R dataframe containing node label, longitude, and latitude
#     label_name - the name of the column for node label
#     lon_name - the name of the column for node longitude 
#     lat_name - the name of the column for node latitude
#     bipartite_name - (optional) the name of the column that indicates the bipartite set of the nodes 
nodes = processNode(NYCMafiaNodes, 'label', 'LonX', 'LatY')
# params:
#     edges - a R dataframe containing source node label and target node label
#     source_name - the name of the column for source node label
#     target_name - the name of the column for target node label
#     weight_name - (optional) the name of the column for edge weight 
edges = processEdge(NYCMafiaEdges, 'Source', 'Target')

#-----calculates network density within a radius (500 meters - Euclidean distance) of each node in a network
# params:
#     nodes - a list of named lists. Each sublist contains a node.
#     edges - a list of list. Each sublist contains an edge.
#     radius - radius (in the coordinate unit) of the scanning window. 
#     (optional) min - minimum number of points required to be in the search window. Default to 3.
# return:
#     list(heat, edgeWithin) - a list of two dataframe for node and edge table. 
heat = NDScanRadius(nodes, edges, 500, 5)[[1]]
edgeWithin = NDScanRadius(nodes, edges, 500, 5)[[2]]
```

#### Other Available Functions

Currently available functions in SSNtools for SSN hotspot detection:

``` r
library(SSNtools)
#-----calculates network density within 10 nearest neighbors of each node in a network
heat = NDScanKNearest(nodes, edges, 10)[[1]]

#-----calculates network density within a radius (500 meters - Manhattan distance) of each node in a network
heat = NDScanManhattan(nodes, edges, 500)[[1]]

#----calculate the density of edges within a radius (500 meters - Euclidean distance) of every node in a graph
heat = edgeScanRadius(nodes, edges, 500)[[1]]

#-----calculates the density of edges within 10 nearest neighbors of each node in a network
heat = edgeScanKNearest(nodes, edges, 10)[[1]]

#-----calculates the density of edges within a radius (500 meters - Manhattan distance) of each node in a network
heat = edgeScanManhattan(nodes, edges, 500)[[1]]
```

#### Application to a Weighted and Bipartite Graph

We have a built-in dataset for POI visits to test the application in a
weighted and bipartite graph. The data from the dataset is processed
from SafeGraph with extra filters and coding to hide sensitive
information. The dataset is meant to be educational and thus can be
inaccurate for real implications. The nodes in the dataset are
restaurants in Atlanta (set 1) and centroids of census block group (set
2). The edges in the dataset are visits from the census block group to
restaurants. The weight of the edges represent the percentage of total
visits coming from a particular census block group. In total there are
**1356 nodes and 7926 edges**. The coordinates system is transformed
with crs=26967.

Based on the definitions of EdgeScan and NDScan, **directed** argument
is only implemented for ND-functions, while **weighted** argument is
only implemented for the Edge-functions.

``` r
library(SSNtools)

data(POINodes)
data(POIEdges)

nodes = processNode(POINodes, 'label', 'LonX', 'LatY', 'Bipartite')
edges = processEdge(POIEdges, 'Source', 'Target', 'Weight')

temp = NDScanRadius(nodes, edges, 1000, min=1, directed=FALSE, bipartite=TRUE)
heat = temp[[1]]
edgeWithin = temp[[2]]

temp = edgeScanRadius(nodes, edges, 1000, min=1, directed=FALSE, bipartite=TRUE)
heat = temp[[1]]
edgeWithin = temp[[2]]
```

#### Future Updates

This package is being developed under the NSF Career Grant: A Research
and Educational Framework for Incorporating Spatial Heterogeneity into
Social Network Analysis. More spatial social network metrics will be
incorporated in the package in the near future.
