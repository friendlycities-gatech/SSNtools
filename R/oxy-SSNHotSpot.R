#------------------------------------------#
#---------------Main Functions ------------#
#------------------------------------------#

#' @title processNode
#' @description process user_nodes to correct format for hotspot detection algorithm
#' @param data user_nodes, which should be a R dataframe that contains a column of node label, a column of node longitude, and a column of node latitude
#' @param label_name column name (string) that contains the node labels
#' @param lon_name column name (float) that contains the node longitude 
#' @param lat_name column name (float) that contains the node latitude
#' @return a list of named lists, in which the name for each list is the node label. The sublist contains all info of a node
#' @details DETAILS
#' @examples 
#' \dontrun{
#' nodes = processNode(user_nodes, 'label', 'lon', 'lat')
#' nodes[[1]] 
#' nodes[['YOUR NODE LABEL']][['lon']]
#' }
#' @rdname processNode
#' @export 
processNode = function(data, label_name, lon_name, lat_name) {
  #convert columns to the right data format
  names(data)[names(data) == label_name] <- "label"
  names(data)[names(data) == lon_name] <- "lon"
  names(data)[names(data) == lat_name] <- "lat"
  data$label = as.character(data$label)
  data$lon = as.numeric(as.character(data$lon))
  data$lat = as.numeric(as.character(data$lat))
  
  #convert data frame to a list of named lists
  data2 = as.list(data)
  nodes = list()
  
  for (i in 1:nrow(data)) {
    temp = list()
    label = as.character(data2$label[i])
    node = list('label' = label, 'lon' = data2$lon[i], 'lat'= data2$lat[i])
    temp[[label]] <- node #this is the only way to assign key by variable name
    nodes = append(nodes, temp)
  }
  return(nodes)
}

#data: R dataframe 
#source_name: a string that indicates the column with the source node label 
#target_name: a string that indicates the column with the target node label
#' @title processEdge
#' @description convert a R dataframe with two columns (Source name and Target name) to correct formats for following algorithms
#' @param data user_edges, which should be a R dataframe that contains a column of source (string of node label) and a column of target (string of node label)
#' @param source_name column name (string) that contains the source node labels
#' @param target_name column name (string) that contains the target node labels
#' @return a list of lists. Each sublist contains all info of an edge
#' @details DETAILS
#' @examples 
#' \dontrun{
#' edges = processEdge(user_edges, 'Source', 'Target')
#' }
#' @rdname processEdge
#' @export 
processEdge = function(data, source_name, target_name) {
  data$Source = as.character(data$Source)
  data$Target = as.character(data$Target)
  
  data2 = as.list(data)
  edges = list()
  
  for (i in 1:nrow(data)) {
    edge = list('Source' = data2$Source[i], 'Target' = data2$Target[i]) #this is the only way to assign key by variable name
    edges[[length(edges) + 1]] = edge
  }
  return(edges)
}

#' @title EdgeScanRadius
#' @description edgeScan - generates heatmap of edges within maxRadius of every node in a graph
#' @param nodes nodes of graph (a list of named lists)
#' @param edges edges of graph (a list of lists)
#' @param maxRadius radius in the unit of coordinates of search window
#' @return a R dataframe that contains a column of node label, and a column of heat associated with the node
#' @details DETAILS
#' @examples 
#' \dontrun{
#' edgeScanRadius(nodes, edges, 500) 
#'  }
#' }
#' @rdname edgeScanRadius
#' @export 
edgeScanRadius = function(nodes, edges, maxRadius) {
  visitedNodes = list()
  labels = c()
  numedges = c()
  
  runningNode = nodes[[1]]
  while (length(visitedNodes) < length(nodes)) { 
    numEdges = getNumEdgesInRange(edges, runningNode, maxRadius)
    
    labels = c(labels, runningNode[['label']])
    numedges = c(numedges, numEdges)
    
    temp = list()
    temp[[runningNode[['label']]]] <- runningNode
    visitedNodes = append(visitedNodes, temp)
    runningNode = nearestUnvisitedNeighbor(nodes, visitedNodes, runningNode)
  }
  heat = data.frame('label' = labels, 'heat' = numedges)
  return(heat)
}

#' @title edgeScanKNearest
#' @description generates heatmap of edges between k nearest nodes to each independent node in a graph
#' @param nodes nodes of graph (a list of named lists)
#' @param edges edges of graph (a list of lists)
#' @param k number of nodes in search window
#' @return a R dataframe that contains a column of node label, and a column of heat associated with the node
#' @details 
#' @examples 
#' \dontrun{
#' edgeScanKNearest(nodes, edges, 10)
#' }
#' @rdname edgeScanKNearest
#' @export 
edgeScanKNearest = function(nodes, edges, k) {
  visitedNodes = list()
  labels = c()
  numedges = c()
  
  runningNode = nodes[[1]]
  while (length(visitedNodes) < length(nodes)) {
    kNearest = nearestNeighbors(nodes, runningNode, k)
    rad = 0 
    for (node in kNearest) {
      if (euclidDistance(node, runningNode) > rad) {
        rad = euclidDistance(node, runningNode)
      }
    }
    numEdges = getNumEdgesInRange(edges, runningNode, rad)
    
    labels = c(labels, runningNode[['label']])
    numedges = c(numedges, numEdges)
    
    temp = list()
    temp[[runningNode[['label']]]] <- runningNode
    visitedNodes = append(visitedNodes, temp)
    runningNode = nearestUnvisitedNeighbor(nodes, visitedNodes, runningNode)
  }
  heat = data.frame('label' = labels, 'heat' = numedges)
  return(heat)
}

# edgeScan - generates heatmap of edges within manhattan distance of every node in a graph
# params:
#     nodes - nodes of graph
#     edges - edges of graph
#     distance - manhattan distance in km of search window
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nodes PARAM_DESCRIPTION
#' @param edges PARAM_DESCRIPTION
#' @param distance PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname edgeScanManhattan
#' @export 
edgeScanManhattan = function(nodes, edges, distance) {
  visitedNodes = list()
  labels = c()
  numedges = c()
  runningNode = nodes[[1]]
  while (length(visitedNodes) < length(nodes)) {
    numEdges = numberEdgesWithinManhattanDistance(nodes, edges, runningNode, distance)
    labels = c(labels, runningNode[['label']])
    numedges = c(numedges, numEdges)
    temp = list()
    temp[[runningNode[['label']]]] <- runningNode
    visitedNodes = append(visitedNodes, temp)
    runningNode = nearestUnvisitedNeighbor(nodes, visitedNodes, runningNode)
  }
  heat = data.frame('label' = labels, 'heat' = numedges)
  return(heat)
}


# NDScan - calculates network density within a radius of each node in a network.
# This is the ratio between the Actual Connections and the Potential Connections in a network.
# 
# Below are the three implementations of this, using radius, K-nearest, and Manhattan distance metrics.
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nodes PARAM_DESCRIPTION
#' @param edges PARAM_DESCRIPTION
#' @param radius PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname NDScanRadius
#' @export 
NDScanRadius = function(nodes, edges, radius) {
  visitedNodes = list()
  labels = c()
  ndensity = c()
  runningNode = nodes[[1]]
  
  while (length(visitedNodes) < length(nodes)) {
    #print(length(visitedNodes)/length(nodes))
    numNodesInRadius = numberNodesWithinRadius(nodes, runningNode, radius)
    potential = numNodesInRadius * (numNodesInRadius - 1)/2
    numEdges = getNumEdgesInRange(edges, runningNode, radius)
    
    if (numNodesInRadius < 3) {
      labels = c(labels, runningNode[['label']])
      ndensity = c(ndensity, 0)
    } else {
      nDensity = numEdges / potential 
      labels = c(labels, runningNode[['label']])
      ndensity = c(ndensity, nDensity)
      if (nDensity > 1) {
        print("Node in Question: ", runningNode)
        print("Num nodes in radius (including source): ", numNodesInRadius)
        print("Num Edges in radius: ", numEdges)
        print("Potential Edges: ", potential)
      }
    }
    temp = list()
    temp[[runningNode[['label']]]] <- runningNode
    visitedNodes = append(visitedNodes, temp)
    runningNode = nearestUnvisitedNeighbor(nodes, visitedNodes, runningNode)
  }
  heat = data.frame('label' = labels, 'heat' = ndensity)
  return(heat)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nodes PARAM_DESCRIPTION
#' @param edges PARAM_DESCRIPTION
#' @param k PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname NDScanKNearest
#' @export 
NDScanKNearest = function(nodes, edges, k) {
  visitedNodes = list()
  labels = c()
  ndensity = c()
  runningNode = nodes[[1]]
  
  while (length(visitedNodes) < length(nodes)) {
    kNearest = nearestNeighbors(nodes, runningNode, k)
    rad = 0
    for (node in kNearest) {
      if (euclidDistance(runningNode, node) > rad) {
        rad = euclidDistance(runningNode, node)
      }
    }
    numEdges = getNumEdgesInRange(edges, runningNode, rad)
    potentialEdges = (k + 1)*k/2
    nDensity = numEdges/potentialEdges
    
    labels = c(labels, runningNode[['label']])
    ndensity = c(ndensity, nDensity)
    
    temp = list()
    temp[[runningNode[['label']]]] <- runningNode
    visitedNodes = append(visitedNodes, temp)
    runningNode = nearestUnvisitedNeighbor(nodes, visitedNodes, runningNode)
  }
  heat = data.frame('label' = labels, 'heat' = ndensity)
  return(heat)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nodes PARAM_DESCRIPTION
#' @param edges PARAM_DESCRIPTION
#' @param distance PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname NDScanManhattan
#' @export 
NDScanManhattan = function(nodes, edges, distance) {
  visitedNodes = list()
  labels = c()
  ndensity = c()
  runningNode = nodes[[1]]
  
  while (length(visitedNodes) < length(nodes)) {
    numNodesInManhattanDistance = numberNodesWithinManhattanDistance(nodes, runningNode, distance)
    edgesInManhattanDistance = getEdgesInManhattanDistance(edges, runningNode, distance)
    #nodesInManhattanDistance = getNodesInManhattanDistance(nodes, runningNode, distance)
    
    potentialEdges = numNodesInManhattanDistance * (numNodesInManhattanDistance - 1) / 2
    numEdges = numberEdgesWithinManhattanDistance(nodes, edges, runningNode, distance)
    
    if (numNodesInManhattanDistance < 3) {
      labels = c(labels, runningNode[['label']])
      ndensity = c(ndensity, 0)
    } else {
      nDensity = numEdges/potentialEdges
      labels = c(labels, runningNode[['label']])
      ndensity = c(ndensity, nDensity)
    }
    temp = list()
    temp[[runningNode[['label']]]] <- runningNode
    visitedNodes = append(visitedNodes, temp)
    runningNode = nearestUnvisitedNeighbor(nodes, visitedNodes, runningNode)
  }
  heat = data.frame('label' = labels, 'heat' = ndensity)
  return(heat)
}
