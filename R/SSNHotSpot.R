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

#To retrieve one node with key
#nodes[[1]]

#To retrieve the values of a node 
#nodes[['YOUR LABEL']][['label']]

#data: R dataframe 
#source_name: a string that indicates the column with the source node label 
#target_name: a string that indicates the column with the target node label
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

#------------------------------------------#
#---------------Main Functions ------------#
#------------------------------------------#

# edgeScan - generates heatmap of edges within maxRadius of every node in a graph
# params:
#     nodes - nodes of graph
#     edges - edges of graph
#     maxRadius - radius in km of search window

edgeScanRadius = function(nodes, edges, maxRadius) {
  visitedNodes = list()
  labels = c()
  numedges = c()
  
  runningNode = nodes[[1]]
  while (length(visitedNodes) < length(nodes)) { 
    numEdges = getNumEdgesInRange(nodes, edges, runningNode, maxRadius)
    
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

# edgeScan - generates heatmap of edges between k nearest nodes to each independent node in a graph
# params:
#     nodes - nodes of graph
#     edges - edges of graph
#     k - number of nodes in search window

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
    numEdges = getNumEdgesInRange(nodes, edges, runningNode, rad)
    
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
NDScanRadius = function(nodes, edges, radius) {
  visitedNodes = list()
  labels = c()
  ndensity = c()
  runningNode = nodes[[1]]
  
  while (length(visitedNodes) < length(nodes)) {
    #print(length(visitedNodes)/length(nodes))
    numNodesInRadius = numberNodesWithinRadius(nodes, runningNode, radius)
    potential = numNodesInRadius * (numNodesInRadius - 1)/2
    numEdges = getNumEdgesInRange(nodes, edges, runningNode, radius)
    
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
    numEdges = getNumEdgesInRange(nodes, edges, runningNode, rad)
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

NDScanManhattan = function(nodes, edges, distance) {
  visitedNodes = list()
  labels = c()
  ndensity = c()
  runningNode = nodes[[1]]
  
  while (length(visitedNodes) < length(nodes)) {
    numNodesInManhattanDistance = numberNodesWithinManhattanDistance(nodes, runningNode, distance)
    edgesInManhattanDistance = getEdgesInManhattanDistance(nodes, edges, runningNode, distance)
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