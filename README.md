
## Advanced Metrics for Analayzing Spatial Social (Non-planar) Networks

**SSNtools** is an R package that provides metrics for analyzing and
visualizing spatial social networks. It is implemented with base R
syntax.

**SSNtools** is currently equipped with three sets of functions:

- **EdgeScan** and **NDScan**: a series of functions to calculate
  hotspots (i.e., heat) of spatial social networks. These functions
  detect the number of non-planar edges and the network density of a
  subset of a social network contained within a focal window. In another
  words, the algorithms return hotspots where nodes are not only densely
  located but also connected.

- **K-fullfillment**: In network logistics, a fulfillment metric
  indicates the extent to which a node’s capacity for supply has been
  met (Li et al. 2019). In this package, this metric is defined as the
  number of a node’s k-nearest neighbors that it is connected to. Here,
  `k` is equal to the node’s degree. Nodes that are exclusively
  connected to their nearest neighbors will have a k-fulfillment value
  of 1.

- **Local network flattening ratio**: This metric (adapted from Sarkar
  et al. 2019) is defined as the ratio of a node’s minimized distance
  (`d_opt`) needed to connect to any k nearest neighbors to the total
  actual distance (`d_act`) of its connections. Nodes with low values
  prioritize distant connections.

Please see the tutorial [Spatial Social Networks (SSN) Visualization and
Metrics with
R](https://friendlycities-gatech.github.io/SSN_tutorial/advanced-ssn-metrics.html)
for more detailed usage demonstration. More advanced metrics will be
implemented in the future.

- [SSN Hostpots
  Detection](https://friendlycities-gatech.github.io/SSN_tutorial/advanced-ssn-metrics.html#ssn-hotspots-detection)
- [K-fullfillment](https://friendlycities-gatech.github.io/SSN_tutorial/advanced-ssn-metrics.html#k-fullfillment)

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

All the main functions (e.g., edgeScanRadius and NDScanRadius) will
return a list of two dataframes. The first dataframe is a node table
that has the node label and the **heat**, presenting the number of edges
in EdgeScan and network density in NDScan respectively. The second
dataframe is an edge table that has the edge pair and a binary column
**edgeWithin** indicating whether the edge is within the scanning
window. This dataframe can be helpful to filter edges for visualization.

``` r
library(SSNtools)

data(NYCMafiaNodes)
data(NYCMafiaEdges)

# ----process dataframe into a list of lists 
# params:
#     data - a R dataframe containing node label, longitude, and latitude
#     label_name - the name of the column for node label
#     lon_name - the name of the column for node longitude 
#     lat_name - the name of the column for node latitude
#     bipartite_name - (optional) the name of the column that indicates the bipartite set of the nodes. The set of nodes that EdgeScan or NDScan should report on should be coded as 1 in the biparite column, and 0 otherwise.  
nodes = processNode(NYCMafiaNodes, 'label', 'LonX', 'LatY')
# params:
#     data - a R dataframe containing source node label and target node label
#     source_name - the name of the column for source node label
#     target_name - the name of the column for target node label
#     weight_name - (optional) the name of the column for edge weight 
edges = processEdge(NYCMafiaEdges, 'Source', 'Target')

#----calculate the density of edges within a radius (500 meters - Euclidean distance) of every node in a graph
# params:
#     nodes - a list of named lists. Each sublist contains a node.
#     edges - a list of list. Each sublist contains an edge.
#     radius - radius (in the coordinate unit) of the scanning window. 
#     (optional) min - minimum number of points required to be in the search window. Default to 3.
#     (optional) weighted - whether the result, number of edges, will be weighted (as sum of edge weights). Default to FALSE.
#     (optional) bipartite - whether the result will be calculated as a bipartite network. Default to FALSE.
# return:
#     list(heat, edgeWithin) - a list of two dataframe for node and edge table. 
heat = edgeScanRadius(nodes, edges, 500)
heat = result[[1]]
edgeWithin = result[[2]]

#-----calculates network density within a radius (500 meters - Euclidean distance) of each node in a network
# params:
#     nodes - a list of named lists. Each sublist contains a node.
#     edges - a list of list. Each sublist contains an edge.
#     radius - radius (in the coordinate unit) of the scanning window. 
#     (optional) min - minimum number of points required to be in the search window. Default to 3.
#     (optional) directed - whether the result, network density, will be calculated as a directed network. Default to FALSE.
#     (optional) bipartite - whether the results will be calculated as a bipartite network. Default to FALSE.
# return:
#     list(heat, edgeWithin) - a list of two dataframe for node and edge table. 
result = NDScanRadius(nodes, edges, 500)
heat = result[[1]]
edgeWithin = result[[2]]
```

The first step `processNode()` and `processEdge()` converts R dataframe
data to a list of named lists. If you are familiar with Python, this
output format resembles object-oriented programming. You can read and
modify data like the following:

``` r
# retrieve the first node
nodes[[1]]

# $label
# [1] "AMAROSA-ALEXANDER"

# $lon
# [1] 302206.2

# $lat
# [1] 57958.61

# retrieve the node list named after "AMAROSA-ALEXANDER"
nodes[["AMAROSA-ALEXANDER"]]

# $label
# [1] "AMAROSA-ALEXANDER"

# $lon
# [1] 302206.2

# $lat
# [1] 57958.61

# retrieve node attributes for node named after "AMAROSA-ALEXANDER"
nodes[["AMAROSA-ALEXANDER"]][['lon']]

# 302206.2

# modify node attributes for node named after "AMAROSA-ALEXANDER"
nodes[["AMAROSA-ALEXANDER"]][['label']] <- 'test'
nodes[["AMAROSA-ALEXANDER"]]

# $label
# [1] "test"

# $lon
# [1] 302206.2

# $lat
# [1] 57958.61
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

#-----calculates network density within (i.e., less than) 500 unit of distance for each node in a network, given a user-defined distance or travel time matrix. The input matrix needs to be a full matrix with column and row names. For example, with three nodes, the matrix should look like the following, with diagnal coded as NA. Noted that 0 has a practical meaning of zero distance that is different from NA. 

#-----example matrix input
#    A1 A2 A3
# A1 NA 0  1
# A2 0  NA 2
# A3 1  2  NA

heat = NDScanMatrix(nodes, edges, 500, matrix)[[1]]

#-----calculates the number of edges within (i.e., less than) 500 units of distance or travel time for each node in a network, given a user-defined distance or travel time matrix.  
heat = edgeScanMatrix(nodes, edges, 500, matrix)[[1]]
```

#### Application to a Weighted and Bipartite Graph

We have a built-in dataset for POI visits to test the application in a
weighted and bipartite graph. The data from the dataset is processed
from SafeGraph with extra filters and coding to hide sensitive
information. The dataset is meant to be educational and thus can be
inaccurate for real implications. The nodes in the dataset are
restaurants in Atlanta (set 1) and centroids of census block group (set
0). The edges in the dataset are visits from the census block group to
restaurants. The weight of the edges represent the percentage of total
visits coming from a particular census block group. You can call
**POINodes (n=1356) and POIEdges (n=7926)** to directly access the
sample dataset. The coordinates system is transformed with crs=26967
(unit meter). If you are using your own dataset, there should be a
bipartite column that indicates which set the node is in. The set that
would like have EdgeScan or NDScan values reported should be coded as 1,
and 0 otherwise. In our **POINodes** sample dataset, restaurant POIs are
coded as 1 and census block group centroids are coded as 0 in the
bipartite network. Please reference the sample dataset if you are
unclear of the input formats.

Based on the definitions of EdgeScan and NDScan, **directed** argument
is only implemented for ND-functions, while **weighted** argument is
only implemented for the Edge-functions.

``` r
library(SSNtools)

data(POINodes)
data(POIEdges)

nodes = processNode(POINodes, 'label', 'LonX', 'LatY', 'Bipartite')
edges = processEdge(POIEdges, 'Source', 'Target', 'Weight')

#takes about 3 mins to run
temp = NDScanKNearest(nodes, edges, 5, directed=FALSE, bipartite=TRUE)
heat = temp[[1]]
edgeWithin = temp[[2]]

#takes about 3 mins to run
temp = edgeScanKNearest(nodes, edges, 5, weighted=TRUE, bipartite=TRUE)
heat = temp[[1]]
edgeWithin = temp[[2]]
```

#### Special notes to use edgeScanMatrix or NDScanMatrix

The input matrix for **edgeScanMatrix** or **NDScanMatrix** function
needs to be a full matrix (i.e., the column and row includes all nodes).
This is still true for bipartite networks and values are required even
for nodes within the same set. For example, in a bipartite network, if
you have 4 nodes and they are A1, A2, B1, B2, then your distance matrix
should be 4 by 4 and self-pairs are coded as NA (see below). It is
important to note that even though B1B2 (or B2B1) has no connections,
they should still be given a distance value (=3) in the distance matrix.
This is because number of edges and network density needs to know all
the nodes within the distance threshold.

``` r
#     A1  A2  B1  B2
# A1  NA  0   1   2  
# A2  0   NA  1   2
# B1  1   1   NA  3
# B2  2   2   3   NA 
```

If needed, you can use the following code snippet to generate a test
matrix for **NYCMafiaNodes**.

``` r
#create a 298*298 matrix filled with values between 1 and 10
n = nrow(NYCMafiaNodes)
m = matrix(sample.int(10, n*n, replace=TRUE), ncol=n)

#assign node labels to column and row names
colnames(m) = NYCMafiaNodes$label[1:n]
rownames(m) = NYCMafiaNodes$label[1:n]

#set values of self pairs to NA
diag(m) <- NA

#make sure matrix values are symmetrical
m[lower.tri(m)] = t(m)[lower.tri(m)]

#use the matrix in edgeScanMatrix function
nodes = processNode(NYCMafiaNodes, 'label', 'LonX', 'LatY')
edges = processEdge(NYCMafiaEdges, 'Source', 'Target')
heat = edgeScanMatrix(nodes, edges, 5, m, min=3)
```

You can use the following code snippet to transform a fully connected
edge table **allEdgesTable** (for example, in a R dataframe, with three
columns: source, target, and walking_distance) into an adjacency matrix
acceptable to **edgeScanMatrix** and **NDScanMatrix**. Note that this
edge table is **DIFFERENT** from the edge table of your network (e.g.,
NYCMafiaEdges): it represents all possible edges (node pairs) and their
associated distances. The size of this table can grow quickly with
network size. For example, for NYCMafia data (n = 298),
**allEdgesTable** has 44,253 rows.

The following codes show how to generate **allEdgesTable** for
**NYCMafiaNodes**.

``` r
allEdgesTable = as.data.frame(t(combn(NYCMafiaNodes$label, 2))) 
colnames(allEdgesTable) <- c('Source', 'Target')
```

The following codes assume **allEdgesTable** already has a column called
**walking_distance** and show how to generate an input matrix for
**edgeScanMatrix**.

``` r
library(igraph)
library(tidyverse)

#assume users generated walking distance for each edge
allEdgesTable$walking_distance <- "USER INPUT" 

#convert allEdgesTable into a network graph; 
#change walking distance that has a practical meaning of 0 to 0.001 because as_adjacency_matrix function defaults to fill in pairs without values with 0. 
g = graph_from_data_frame(allEdgesTable %>% mutate(walking_distance == 0, 0.001, walking_distance), 
directed=FALSE, vertices=nodeTable)
mat = as_adjacency_matrix(g, sparse=F, attr="walking_distance")

#Since 0 has practical meaning, we would like cells with no distance values to be coded as NA, and cells that have 0 distance (changed to 0.001 above) to be coded as 0. 
mat[mat==0]<-NA
mat[mat==0.001]<-0

#use the matrix in edgeScanMatrix function
nodes = processNode(NYCMafiaNodes, 'label', 'LonX', 'LatY')
edges = processEdge(NYCMafiaEdges, 'Source', 'Target')
heat = edgeScanMatrix(nodes, edges, 1000, mat, min=3)
```

#### Run time and optimization

Note that, existing functions are not optimized for large-scale spatial
social networks. As a benchmark, the application of **NDScanRadius** on
**NYCMafiaNodes** (n=298) and **NYCMafiaEdges**(n=946) returns results
in 0.5s, and the same function on **POINodes (n=1356)** and **POIEdges
(n=7926)** returns results in 3m. The time increases exponentially with
the size of the network.

Among all the functions, **edgeScanMatrix** and **NDScanMatrix**
functions are implemented in the most time-efficient manner (matrix
operation rather than list operation). Thus, if you really need to
optimize run time, you can use these two functions, and generate your
own Euclidean, Manhattan, or KNN matrices (just like distance matrix).

#### Future Updates

This package is being developed under the NSF Career Grant: A Research
and Educational Framework for Incorporating Spatial Heterogeneity into
Social Network Analysis. More spatial social network metrics will be
incorporated in the package in the near future.

#### Reference

Liang, X., Baker, J., DellaPosta, D., & Andris, C. (2023). Is your
neighbor your friend? Scan methods for spatial social network hotspot
detection. *Transctions in GIS*. <https://doi.org/10.1111/tgis.13050>

Andris, C., DellaPosta, D., Freelin, B. N., Zhu, X., Hinger, B., & Chen,
H. (2021). To racketeer among neighbors: spatial features of criminal
collaboration in the American Mafia. *International Journal of
Geographical Information Science*, 35(12), 2463-2488.

Sarkar, D., Andris, C., Chapman, C. A., & Sengupta, R. (2019). Metrics
for characterizing network structure and node importance in Spatial
Social Networks. *International Journal of Geographical Information
Science*, 33(5), 1017-1039.
