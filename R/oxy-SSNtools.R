#------------------------------------------#
#---------------Main Functions ------------#
#------------------------------------------#

#' @title processNode
#' @description process user_nodes to correct format for hotspot detection algorithm
#' @param data user_nodes, which should be a R dataframe that contains a column of node label, a column of node longitude, and a column of node latitude
#' @param label_name column name (string) that contains the node labels
#' @param lon_name column name (float) that contains the node longitude 
#' @param lat_name column name (float) that contains the node latitude
#' @param bipartite_name (optional) column name (binary) that contains which set the node is in if available. 1 represents the set that the heat is visualized on.
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
processNode = function(data, label_name, lon_name, lat_name, bipartite_name = NA) {
  #convert columns to the right data format
  names(data)[names(data) == label_name] <- "label"
  names(data)[names(data) == lon_name] <- "lon"
  names(data)[names(data) == lat_name] <- "lat"
  data$label = as.character(data$label)
  data$lon = as.numeric(as.character(data$lon))
  data$lat = as.numeric(as.character(data$lat))
  
  if(!is.na(bipartite_name)) {
    names(data)[names(data) == bipartite_name] <- "bipartite"
    data$bipartite = as.numeric(as.character(data$bipartite))
  }
  
  #convert data frame to a list of named lists
  data2 = as.list(data)
  nodes = list()
  
  for (i in 1:nrow(data)) { 
    temp = list()
    label = as.character(data2$label[i])
    if(is.na(bipartite_name)) {
      node = list('label' = label, 'lon' = data2$lon[i], 'lat'= data2$lat[i])
    } else {
      if(is.null(data2$bipartite[i])) {
        stop('Node bipartite value is not available. Please check if node table contains bipartite values and if the name of the bipartite column is provided in the processNode function')
      }
      node = list('label' = label, 'lon' = data2$lon[i], 'lat'= data2$lat[i], 'bipartite' = data2$bipartite[i])
    }
    
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
#' @param weight_name (optional) column name (float) that contains edge weight if available
#' @return a list of lists. Each sublist contains all info of an edge
#' @details DETAILS
#' @examples 
#' \dontrun{
#' edges = processEdge(user_edges, 'Source', 'Target')
#' }
#' @rdname processEdge
#' @export 
processEdge = function(data, source_name, target_name, weight_name = NA) {
  
  data2 = as.list(data)
  edges = list()
  data2[[source_name]] = as.character(data2[[source_name]])
  data2[[target_name]] = as.character(data2[[target_name]])
  for (i in 1:nrow(data)) {
    if(is.na(weight_name)) {
      edge = list('Source' = data2[[source_name]][i], 'Target' = data2[[target_name]][i])
    } else {
      if(is.null(data2[[weight_name]][i])) {
        stop('Edge weight is not available. Please check if edge table contains a weight column and if the name of the weight column is provided in the processEdge function')
      }
      edge = list('Source' = data2[[source_name]][i], 'Target' = data2[[target_name]][i], 'Weight' = data2[[weight_name]][i])
    }
    edges[[length(edges) + 1]] = edge
  }
  return(edges)
}

#' @title data
#' @description load a built-in dataset
#' @param data the name of the built-in dataset
#' @return the built-in dataset
#' @details DETAILS
#' #' @examples 
#' \dontrun{
#' data(NYCMafiaNodes) 
#'  }
data = function(data) {
  return(data)
}

#' @title EdgeScanRadius
#' @description edgeScan - generates heatmap of edges within maxRadius of every node in a graph
#' @param nodes nodes of graph (a list of named lists)
#' @param edges edges of graph (a list of lists)
#' @param radius radius in the unit of coordinates of search window
#' @param min (optional) minimum number of nodes in the distance window
#' @param weighted (optional) boolean value of whether a weighted column has been included.
#' @param bipartite (optional) boolean value of whether the data is a bipartite network
#' @return a list of two dataframes. The first R datafrmae contains a column of node label, and a column of heat associated with the node. The second R dataframe contains the edge pairs and a boolean column indicating whether the edge is within the scanning window.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' edgeScanRadius(nodes, edges, 500) 
#'  }
#' @rdname edgeScanRadius
#' @export 
edgeScanRadius = function(nodes, edges, radius, min=3, weighted=FALSE, bipartite=FALSE) {
  if(!inherits(nodes, "list") | !inherits(edges, "list")) {
    stop('nodes or edges arguments only intake a list of lists. Please use processNode or processEdge functions to convert R dataframe to a list of lists')
  }
  
  labels = c()
  numedges = c()
  
  if(bipartite) {
    if (is.null(nodes[[1]][['bipartite']])) {
      stop('Node bipartite value is not available. Please check if node table contains a bipartite column and if the name of the bipartite column is provided in the processNode function')}
    #sort so that bipartite == 1 is on top.
    nodes = nodes[order(-sapply(nodes, function(x) x[['bipartite']]))] 
    stop = length(Filter(function(x) all((x <- x$bipartite == 1)), nodes))
  } else {
    stop = length(nodes)
  }
  
  for (i in seq(1, stop)) {
    numNodesInRadius = numberNodesWithinRadius(nodes, nodes[[i]], radius, bipartite)
    
    if (numNodesInRadius < min) {
      labels = c(labels, nodes[[i]][['label']])
      numedges = c(numedges, NA)
    } else {
      numEdges = getNumEdgesInRange(nodes, edges, nodes[[i]], radius, weighted)
      labels = c(labels, nodes[[i]][['label']])
      numedges = c(numedges, numEdges)
    }
  }
  
  heat = data.frame('label' = labels, 'heat' = numedges)
  if(abs(nodes[[1]][['lat']]) <= 180) {
    warning("Distance may be calculated in the degree coordinates, which may need to be projected into other distance units")
  }
  
  source = c()
  target = c()
  weight = c()
  withinwindow = c()
  
  for (edge in edges) {
    source = c(source, edge[['Source']])
    target = c(target, edge[['Target']])
    if(weighted) {weight = c(weight, edge[['Weight']])}
    if (euclidDistance(nodes[[edge[['Source']]]], nodes[[edge[['Target']]]]) < radius) {
      withinwindow = c(withinwindow, 1) 
    } else {
      withinwindow = c(withinwindow, 0) 
    }
  }
  
  if(weighted) {
    edgeWithin = data.frame('Source' = source, 'Target' = target, 'Weight' = weight, 'WithinWindow' = withinwindow)
  } else {
    edgeWithin = data.frame('Source' = source, 'Target' = target, 'WithinWindow' = withinwindow)
  }
  
  return(list(heat, edgeWithin))
}

#' @title edgeScanKNearest
#' @description generates heatmap of edges between k nearest nodes to each independent node in a graph
#' @param nodes nodes of graph (a list of named lists)
#' @param edges edges of graph (a list of lists)
#' @param k number of nodes in search window
#' @param weighted (optional) boolean value of whether a weighted column has been included.
#' @param bipartite (optional) boolean value of whether the data is a bipartite network
#' @return a list of two dataframes. The first R datafrmae contains a column of node label, and a column of heat associated with the node. The second R dataframe contains the edge pairs and a boolean column indicating whether the edge is within the scanning window.
#' @details 
#' @examples 
#' \dontrun{
#' edgeScanKNearest(nodes, edges, 5)
#' }
#' @rdname edgeScanKNearest
#' @export 
edgeScanKNearest = function(nodes, edges, k, weighted=FALSE, bipartite=FALSE) {
  if(!inherits(nodes, "list") | !inherits(edges, "list")) {
    stop('nodes or edges arguments only intake a list of lists. Please use processNode or processEdge functions to convert R dataframe to a list of lists')
  }
  
  labels = c()
  numedges = c()
  
  if(bipartite) {
    if (is.null(nodes[[1]][['bipartite']])) {
      stop('Node bipartite value is not available. Please check if node table contains a bipartite column and if the name of the bipartite column is provided in the processNode function')}
    #sort so that bipartite == 1 is on top.
    nodes = nodes[order(-sapply(nodes, function(x) x[['bipartite']]))] 
    stop = length(Filter(function(x) all((x <- x$bipartite == 1)), nodes))
  } else {
    stop = length(nodes)
  }
  
  for (i in seq(1, stop)) {
    kNearest = nearestNeighbors(nodes, nodes[[i]], k, bipartite) 
    rad = 0 
    for (node in kNearest) {
      if (euclidDistance(node, nodes[[i]]) > rad) {
        rad = euclidDistance(node, nodes[[i]])
      }
    }
    numEdges = getNumEdgesInRange(nodes, edges, nodes[[i]], rad, weighted)
    
    labels = c(labels, nodes[[i]][['label']])
    numedges = c(numedges, numEdges)
  }
  
  heat = data.frame('label' = labels, 'heat' = numedges)
  
  source = c()
  target = c()
  weight = c()
  withinwindow = c()
  
  for (edge in edges) {
    source = c(source, edge[['Source']])
    target = c(target, edge[['Target']])
    if(weighted) {weight = c(weight, edge[['Weight']])}
    #bipartite assumes source node is the one with heat values
    if(bipartite) {
      if (edge[['Target']] %in% sapply(nearestNeighbors(nodes, nodes[[edge[['Source']]]], k, bipartite), function(x) x[['label']])) {
        withinwindow = c(withinwindow, 1) 
      } else {
        withinwindow = c(withinwindow, 0) 
      }
    } else {
      if (edge[['Target']] %in% sapply(nearestNeighbors(nodes, nodes[[edge[['Source']]]], k, bipartite), function(x) x[['label']]) | 
          edge[['Source']] %in% sapply(nearestNeighbors(nodes, nodes[[edge[['Target']]]], k, bipartite), function(x) x[['label']])) {
        withinwindow = c(withinwindow, 1) 
      } else {
        withinwindow = c(withinwindow, 0) 
      }
    }
    
  }
  
  if(weighted) {
    edgeWithin = data.frame('Source' = source, 'Target' = target, 'Weight' = weight, 'WithinWindow' = withinwindow)
  } else {
    edgeWithin = data.frame('Source' = source, 'Target' = target, 'WithinWindow' = withinwindow)
  }
  return(list(heat, edgeWithin))
}

#' @title edgeScanManhattan
#' @description FUNCTION_DESCRIPTION
#' @param nodes PARAM_DESCRIPTION
#' @param edges PARAM_DESCRIPTION
#' @param radius PARAM_DESCRIPTION
#' @param min PARAM_DESCRIPTION
#' @param weighted (optional) boolean value of whether a weighted column has been included.
#' @param bipartite (optional) boolean value of whether the data is a bipartite network
#' @return a list of two dataframes. The first R datafrmae contains a column of node label, and a column of heat associated with the node. The second R dataframe contains the edge pairs and a boolean column indicating whether the edge is within the scanning window.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#' }
#' @rdname edgeScanManhattan
#' @export 
edgeScanManhattan = function(nodes, edges, radius, min=3, weighted=FALSE, bipartite=FALSE) {
  if(!inherits(nodes, "list") | !inherits(edges, "list")) {
    stop('nodes or edges arguments only intake a list of lists. Please use processNode or processEdge functions to convert R dataframe to a list of lists')
  }
  
  labels = c()
  numedges = c()
  
  if(bipartite) {
    if (is.null(nodes[[1]][['bipartite']])) {
      stop('Node bipartite value is not available. Please check if node table contains a bipartite column and if the name of the bipartite column is provided in the processNode function')}
    #sort so that bipartite == 1 is on top.
    nodes = nodes[order(-sapply(nodes, function(x) x[['bipartite']]))] 
    stop = length(Filter(function(x) all((x <- x$bipartite == 1)), nodes))
  } else {
    stop = length(nodes)
  }
  
  for (i in seq(1, stop)) {
    numNodesInManhattanDistance = numberNodesWithinManhattanDistance(nodes, nodes[[i]], radius, bipartite)
    
    if (numNodesInManhattanDistance < min) {
      labels = c(labels, nodes[[i]][['label']])
      numedges = c(numedges, NA)
    } else {
      numEdges = numberEdgesWithinManhattanDistance(nodes, edges, nodes[[i]], radius, weighted)
      labels = c(labels, nodes[[i]][['label']])
      numedges = c(numedges, numEdges)
    }
  }
  
  heat = data.frame('label' = labels, 'heat' = numedges)
  if(abs(nodes[[1]][['lat']]) <= 180) {
    warning("Distance may be calculated in the degree coordinates, which may need to be projected into other distance units")
  }
  
  source = c()
  target = c()
  weight = c()
  withinwindow = c()
  
  for (edge in edges) {
    source = c(source, edge[['Source']])
    target = c(target, edge[['Target']])
    if(weighted) {weight = c(weight, edge[['Weight']])}
    if (ManhattanDistance(nodes[[edge[['Source']]]], nodes[[edge[['Target']]]]) < radius) {
      withinwindow = c(withinwindow, 1) 
    } else {
      withinwindow = c(withinwindow, 0) 
    }
  }
  
  if(weighted) {
    edgeWithin = data.frame('Source' = source, 'Target' = target, 'Weight' = weight, 'WithinWindow' = withinwindow)
  } else {
    edgeWithin = data.frame('Source' = source, 'Target' = target, 'WithinWindow' = withinwindow)
  }
  
  return(list(heat, edgeWithin))
}


# Below are the three implementations of this, using radius, K-nearest, and Manhattan distance metrics.
#' @title NDScanRadius
#' @description FUNCTION_DESCRIPTION
#' @param nodes PARAM_DESCRIPTION
#' @param edges PARAM_DESCRIPTION
#' @param radius PARAM_DESCRIPTION
#' @param min PARAM_DESCRIPTION
#' @param directed (optional) boolean value of whether network density should be calculated as a directed graph.
#' @param bipartite (optional) boolean value of whether the data is a bipartite network
#' @return a list of two dataframes. The first R datafrmae contains a column of node label, and a column of heat associated with the node. The second R dataframe contains the edge pairs and a boolean column indicating whether the edge is within the scanning window.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname NDScanRadius
#' @export 
NDScanRadius = function(nodes, edges, radius, min=3, directed=FALSE, bipartite=FALSE) {
  if(!inherits(nodes, "list") | !inherits(edges, "list")) {
    stop('nodes or edges arguments only intake a list of lists. Please use processNode or processEdge functions to convert R dataframe to a list of lists')
  }
  
  labels = c()
  ndensity = c()
  
  if(bipartite) {
    if (is.null(nodes[[1]][['bipartite']])) {
      stop('Node bipartite value is not available. Please check if node table contains a bipartite column and if the name of the bipartite column is provided in the processNode function')}
    #sort so that bipartite == 1 is on top.
    nodes = nodes[order(-sapply(nodes, function(x) x[['bipartite']]))] 
    stop = length(Filter(function(x) all((x <- x$bipartite == 1)), nodes))
  } else {
    stop = length(nodes)
  }
  
  for (i in seq(1, stop)) {
    numNodesInRadius = numberNodesWithinRadius(nodes, nodes[[i]], radius, bipartite)
    if (numNodesInRadius < min) {
      labels = c(labels, nodes[[i]][['label']])
      ndensity = c(ndensity, NA)
    } else {
      if(bipartite) {
        if(directed) {dir = 2} else {dir = 1}
        potential = numNodesInRadius * (numberNodesWithinRadius(nodes, nodes[[i]], radius, FALSE) - numNodesInRadius) * dir
      } else {
        if(directed) {dir = 1} else {dir = 2}
        potential = numNodesInRadius * (numNodesInRadius - 1)/dir
      }
      numEdges = getNumEdgesInRange(nodes, edges, nodes[[i]], radius, FALSE) #weighted = FALSE for network density
      nDensity = numEdges / potential 
      labels = c(labels, nodes[[i]][['label']])
      ndensity = c(ndensity, nDensity)
      if (nDensity > 1) {stop(paste0('Node', i, ' network density is greater than 1'))}
    }
  }
  
  heat = data.frame('label' = labels, 'heat' = ndensity)
  if(abs(nodes[[1]][['lat']]) <= 180) {
    warning("Distance may be calculated in the degree coordinates, which may need to be projected into other distance units")
  }
  
  source = c()
  target = c()
  withinwindow = c()
  
  for (edge in edges) {
    source = c(source, edge[['Source']])
    target = c(target, edge[['Target']])
    if (euclidDistance(nodes[[edge[['Source']]]], nodes[[edge[['Target']]]]) < radius) {
      withinwindow = c(withinwindow, 1) 
    } else {
      withinwindow = c(withinwindow, 0) 
    }
  }
  
  edgeWithin = data.frame('Source' = source, 'Target' = target, 'WithinWindow' = withinwindow)
  
  return(list(heat, edgeWithin))
}

#' @title NDScanKNearest
#' @description FUNCTION_DESCRIPTION
#' @param nodes PARAM_DESCRIPTION
#' @param edges PARAM_DESCRIPTION
#' @param k PARAM_DESCRIPTION
#' @param directed (optional) boolean value of whether network density should be calculated as a directed graph.
#' @param bipartite (optional) boolean value of whether the data is a bipartite network
#' @return a list of two dataframes. The first R datafrmae contains a column of node label, and a column of heat associated with the node. The second R dataframe contains the edge pairs and a boolean column indicating whether the edge is within the scanning window.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname NDScanKNearest
#' @export 
NDScanKNearest = function(nodes, edges, k, directed=FALSE, bipartite=FALSE) {
  if(!inherits(nodes, "list") | !inherits(edges, "list")) {
    stop('nodes or edges arguments only intake a list of lists. Please use processNode or processEdge functions to convert R dataframe to a list of lists')
  }
  
  labels = c()
  ndensity = c()
  
  if(bipartite) {
    if (is.null(nodes[[1]][['bipartite']])) {
      stop('Node bipartite value is not available. Please check if node table contains a bipartite column and if the name of the bipartite column is provided in the processNode function')}
    #sort so that bipartite == 1 is on top.
    nodes = nodes[order(-sapply(nodes, function(x) x[['bipartite']]))] 
    stop = length(Filter(function(x) all((x <- x$bipartite == 1)), nodes))
  } else {
    stop = length(nodes)
  }
  
  for (i in seq(1, stop)) {
    kNearest = nearestNeighbors(nodes, nodes[[i]], k, bipartite)
    rad = 0
    for (node in kNearest) {
      if (euclidDistance(nodes[[i]], node) > rad) {
        rad = euclidDistance(nodes[[i]], node)
      }
    }
    numNodesInRadius = numberNodesWithinRadius(nodes, nodes[[i]], rad, bipartite)
    if(bipartite) {
      if(directed) {dir = 2} else {dir = 1}
      potential = numNodesInRadius * (numberNodesWithinRadius(nodes, nodes[[i]], rad, FALSE) - numNodesInRadius) * dir
    } else {
      if(directed) {dir = 1} else {dir = 2}
      potential = numNodesInRadius * (numNodesInRadius - 1)/dir
    }
    numEdges = getNumEdgesInRange(nodes, edges, nodes[[i]], rad, FALSE) #weighted = FALSE for network density
    nDensity = numEdges / potential 
    labels = c(labels, nodes[[i]][['label']])
    ndensity = c(ndensity, nDensity)
    if (nDensity > 1) {stop(paste0('Node', i, ' network density is greater than 1'))}
  }
  
  heat = data.frame('label' = labels, 'heat' = ndensity)
  
  source = c()
  target = c()
  withinwindow = c()
  
  for (edge in edges) {
    source = c(source, edge[['Source']])
    target = c(target, edge[['Target']])
    #bipartite assumes source node is the one with heat values
    if(bipartite) {
      if (edge[['Target']] %in% sapply(nearestNeighbors(nodes, nodes[[edge[['Source']]]], k, bipartite), function(x) x[['label']])) {
        withinwindow = c(withinwindow, 1) 
      } else {
        withinwindow = c(withinwindow, 0) 
      }
    } else {
      if (edge[['Target']] %in% sapply(nearestNeighbors(nodes, nodes[[edge[['Source']]]], k, bipartite), function(x) x[['label']]) | 
          edge[['Source']] %in% sapply(nearestNeighbors(nodes, nodes[[edge[['Target']]]], k, bipartite), function(x) x[['label']])) {
        withinwindow = c(withinwindow, 1) 
      } else {
        withinwindow = c(withinwindow, 0) 
      }
    }
    
  }
  
  edgeWithin = data.frame('Source' = source, 'Target' = target, 'WithinWindow' = withinwindow)
  
  return(list(heat, edgeWithin))
}

#' @title NDScanManhattan
#' @description FUNCTION_DESCRIPTION
#' @param nodes PARAM_DESCRIPTION
#' @param edges PARAM_DESCRIPTION
#' @param radius PARAM_DESCRIPTION
#' @param min PARAM_DESCRIPTION
#' @param directed (optional) boolean value of whether network density should be calculated as a directed graph.
#' @param bipartite (optional) boolean value of whether the data is a bipartite network
#' @return a list of two dataframes. The first R datafrmae contains a column of node label, and a column of heat associated with the node. The second R dataframe contains the edge pairs and a boolean column indicating whether the edge is within the scanning window.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname NDScanManhattan
#' @export 
NDScanManhattan = function(nodes, edges, radius, min=3, directed=FALSE, bipartite=FALSE) {
  if(!inherits(nodes, "list") | !inherits(edges, "list")) {
    stop('nodes or edges arguments only intake a list of lists. Please use processNode or processEdge functions to convert R dataframe to a list of lists')
  }
  
  labels = c()
  ndensity = c()
  
  if(bipartite) {
    if (is.null(nodes[[1]][['bipartite']])) {
      stop('Node bipartite value is not available. Please check if node table contains a bipartite column and if the name of the bipartite column is provided in the processNode function')}
    #sort so that bipartite == 1 is on top.
    nodes = nodes[order(-sapply(nodes, function(x) x[['bipartite']]))] 
    stop = length(Filter(function(x) all((x <- x$bipartite == 1)), nodes))
  } else {
    stop = length(nodes)
  }
  
  for (i in seq(1, stop)) {
    numNodesInManhattanDistance = numberNodesWithinManhattanDistance(nodes, nodes[[i]], radius, bipartite)
    if (numNodesInManhattanDistance < min) {
      labels = c(labels, nodes[[i]][['label']])
      ndensity = c(ndensity, NA)
    } else {
      if(bipartite) {
        if(directed) {dir = 2} else {dir = 1}
        potential = numNodesInManhattanDistance * (numberNodesWithinManhattanDistance(nodes, nodes[[i]], radius, FALSE) - numNodesInManhattanDistance) * dir
      } else {
        if(directed) {dir = 1} else {dir = 2}
        potential = numNodesInManhattanDistance * (numNodesInManhattanDistance - 1)/dir
      }
      numEdges = numberEdgesWithinManhattanDistance(nodes, edges, nodes[[i]], radius, FALSE)
      nDensity = numEdges / potential 
      labels = c(labels, nodes[[i]][['label']])
      ndensity = c(ndensity, nDensity)
      if (nDensity > 1) {stop(paste0('Node', i, ' network density is greater than 1'))}
    }
  }
  
  heat = data.frame('label' = labels, 'heat' = ndensity)
  if(abs(nodes[[1]][['lat']]) <= 180) {
    warning("Distance may be calculated in the degree coordinates, which may need to be projected into other distance units")
  }
  source = c()
  target = c()
  withinwindow = c()
  
  for (edge in edges) {
    source = c(source, edge[['Source']])
    target = c(target, edge[['Target']])
    if (ManhattanDistance(nodes[[edge[['Source']]]], nodes[[edge[['Target']]]]) < radius) {
      withinwindow = c(withinwindow, 1) 
    } else {
      withinwindow = c(withinwindow, 0) 
    }
  }
  
  edgeWithin = data.frame('Source' = source, 'Target' = target, 'WithinWindow' = withinwindow)
  
  return(list(heat, edgeWithin))
}