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

#To retrieve one node with key
#nodes[[1]]

#To retrieve the values of a node 
#nodes[['YOUR LABEL']][['label']]

#data: R dataframe 
#source_name: a string that indicates the column with the source node label 
#target_name: a string that indicates the column with the target node label
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

#------------------------------------------#
#---------------Main Functions ------------#
#------------------------------------------#

# edgeScan - generates heatmap of edges within radius of every node in a graph
# params:
#     nodes - nodes of graph
#     edges - edges of graph
#     radius - radius in km of search window

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

# edgeScan - generates heatmap of edges between k nearest nodes to each independent node in a graph
# params:
#     nodes - nodes of graph
#     edges - edges of graph
#     k - number of nodes in search window

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

# edgeScan - generates heatmap of edges within manhattan distance of every node in a graph
# params:
#     nodes - nodes of graph
#     edges - edges of graph
#     radius - manhattan distance in km of search window
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

# NDScan - calculates network density within a radius of each node in a network.
# This is the ratio between the Actual Connections and the Potential Connections in a network.
# 
# Below are the three implementations of this, using radius, K-nearest, and Manhattan distance metrics.
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
