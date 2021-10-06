
rmarkdown::render(“README.Rmd”)

## Hotspot Detection for Spatial Social (Non-planar) Networks

**SSNHotSpot** is an R package that calculate hotspots (i.e., heat) of
spatial social networks.

See paper [Spatial Social Networks (SSN) Hot Spot Detection: Scan
Methods for Non-planar Networks](https://arxiv.org/pdf/2011.07702.pdf)
for detailed methodology.

#### Installation

You can install the released version of SSNHotSpot from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("SSNHotSpot")
```

And the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("friendlycities-gatech/SSNHotSpot")
```

#### Example

This is a basic example which shows you how to use the main functions in
SSNHotSpot. The package comes with an example dataset, which is a
spatial social network of Mafia members in New York City in the 1960s.
See
[here](https://drive.google.com/file/d/1guVURnryYUyXaJ3A7SoMFMpkv7CUx6He/view)
for more details about the dataset. You can call **example\_nodes** and
**example\_edges** to directly access the sample dataset.

``` r
library(SSNHotSpot)
#process dataframe into a list of lists 
nodes = processNode(example_nodes, 'label', 'lon', 'lat')
edges = processEdge(example_edges, 'Source', 'Target')

#Calculates network density within a radius (500 meters) of each node in a network
result = NDScanRadius(nodes, edges, 500)
```