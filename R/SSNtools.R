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
    #order so that bipartite = 1 is on top
    data = data[order(data$bipartite, decreasing=T),]
  }
  
  #convert data frame to a list of named lists
  # Apply the process_row function to each row of the data frame
  nodes <- lapply(split(data, seq(nrow(data))), processNodeHelper, bipartite_name)
  
  # Merge the lists using do.call
  nodes <- do.call(c, nodes)
  
  # Remove the prefixes added by the split() function
  names(nodes) <- gsub("^[0-9]+\\.", "", names(nodes))
  
  #old version with for loop
  #convert data frame to a list of named lists
  # data2 = as.list(data)
  # nodes = list()
  # 
  # for (i in 1:nrow(data)) { 
  #   temp = list()
  #   label = as.character(data2$label[i])
  #   if(is.na(bipartite_name)) {
  #     node = list('label' = label, 'lon' = data2$lon[i], 'lat'= data2$lat[i])
  #   } else {
  #     if(is.null(data2$bipartite[i])) {
  #       stop('Node bipartite value is not available. Please check if node table contains bipartite values and if the name of the bipartite column is provided in the processNode function')
  #     }
  #     node = list('label' = label, 'lon' = data2$lon[i], 'lat'= data2$lat[i], 'bipartite' = data2$bipartite[i])
  #   }
  #   
  #   temp[[label]] <- node #this is the only way to assign key by variable name
  #   nodes = append(nodes, temp)
  # }
  return(nodes)
}

#data: R dataframe 
#source_name: a string that indicates the column with the source node label 
#target_name: a string that indicates the column with the target node label
processEdge = function(data, source_name, target_name, weight_name = NA) {
  #convert columns to the right data format
  names(data)[names(data) == source_name] <- "Source"
  names(data)[names(data) == target_name] <- "Target"
  edges <- lapply(split(data, seq_len(nrow(data))), processEdgeHelper, source_name, target_name, weight_name)
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
      if (edge[['Target']] %in% names(nearestNeighbors(nodes, nodes[[edge[['Target']]]], k, bipartite))) {
        withinwindow = c(withinwindow, 1) 
      } else {
        withinwindow = c(withinwindow, 0) 
      }
    } else {
      if (edge[['Target']] %in% names(nearestNeighbors(nodes, nodes[[edge[['Target']]]], k, bipartite)) | 
          edge[['Source']] %in% names(nearestNeighbors(nodes, nodes[[edge[['Target']]]], k, bipartite))) {
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

edgeScanMatrix = function(nodes, edges, thres, matrix, min=3, weighted=FALSE, bipartite=FALSE) {
  if(!inherits(nodes, "list") | !inherits(edges, "list")) {
    stop('nodes or edges arguments only intake a list of lists. Please use processNode or processEdge functions to convert R dataframe to a list of lists')
  }
  if(!inherits(matrix, "matrix")) {
    stop('Your matrix input is not recognized as a matrix in R. Please check R matrix formats and make sure you have row and column names for the matrix.')
  }
  if(!is.null(nodes[[1]][['bipartite']]) & !bipartite) {
    stop('Your data has a bipartite column, but your bipartite argument is set to FALSE. Please set your bipartite argument to TRUE')
  }
  
  labels = c()
  numedges = c()
  
  if(bipartite) {
    if (is.null(nodes[[1]][['bipartite']])) {
      stop('Node bipartite value is not available. Please check if node table contains a bipartite column and if the name of the bipartite column is provided in the processNode function')}
    #sort so that bipartite == 1 is on top.
    nodes = nodes[order(-sapply(nodes, function(x) x[['bipartite']]))] 
    stop = length(Filter(function(x) all((x <- x$bipartite == 1)), nodes))
    #calculate the number of nodes with bipartite == 1
    bipartite_num = sum(as.numeric(unlist(nodes)[grepl(pattern='bipartite', names(unlist(nodes)))]))
    bipartite_trans_matrix = matrix
    #assign node pairs in the same set with values of 0
    bipartite_trans_matrix[1:bipartite_num, 1:bipartite_num] <- NA
    bipartite_trans_matrix[(bipartite_num+1):length(nodes), (bipartite_num+1):length(nodes)] <- NA
  } else {
    stop = length(nodes)
  }
  
  for (i in seq(1, stop)) {
    NodesInMatrix = NodesWithinMatrixThres(nodes[[i]][['label']], thres, matrix) #NodesInMatrix includes POI and centroids
    if(bipartite) {
      CentroidsInMatrix = NodesWithinMatrixThres(nodes[[i]][['label']], thres, bipartite_trans_matrix) #CentroidsInMatrix only includes centroids
      numNodesInMatrix = length(CentroidsInMatrix)  
    } else {
      numNodesInMatrix = length(NodesInMatrix)  
    }
    if (numNodesInMatrix < min) {
      labels = c(labels, nodes[[i]][['label']])
      numedges = c(numedges, NA)
    } else {
      numEdges = getNumEdgesInMatrix(nodes[[i]][['label']], names(NodesInMatrix), edges, matrix, thres, weighted)
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
    
    if(bipartite) {
      nameWithinMatrix=names(NodesWithinMatrixThres(edge[['Source']], thres, bipartite_trans_matrix))
    } else {
      nameWithinMatrix=names(NodesWithinMatrixThres(edge[['Source']], thres, matrix))
    }
    
    if (edge[['Target']] %in% nameWithinMatrix) {
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
      if (edge[['Target']] %in% names(nearestNeighbors(nodes, nodes[[edge[['Source']]]], k, bipartite))) {
        withinwindow = c(withinwindow, 1) 
      } else {
        withinwindow = c(withinwindow, 0) 
      }
    } else {
      if (edge[['Target']] %in% names(nearestNeighbors(nodes, nodes[[edge[['Source']]]], k, bipartite)) | 
          edge[['Source']] %in% names(nearestNeighbors(nodes, nodes[[edge[['Target']]]], k, bipartite))) {
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

NDScanMatrix = function(nodes, edges, thres, matrix, min=3, directed=FALSE, bipartite=FALSE) {
  if(!inherits(nodes, "list") | !inherits(edges, "list")) {
    stop('nodes or edges arguments only intake a list of lists. Please use processNode or processEdge functions to convert R dataframe to a list of lists')
  }
  if(!inherits(matrix, "matrix")) {
    stop('Your matrix input is not recognized as a matrix in R. Please check R matrix formats and make sure you have row and column names for the matrix.')
  }
  if(!is.null(nodes[[1]][['bipartite']]) & !bipartite) {
    stop('Your data has a bipartite column, but your bipartite argument is set to FALSE. Please set your bipartite argument to TRUE')
  }
  
  labels = c()
  ndensity = c()
  
  if(bipartite) {
    if (is.null(nodes[[1]][['bipartite']])) {
      stop('Node bipartite value is not available. Please check if node table contains a bipartite column and if the name of the bipartite column is provided in the processNode function')}
    #sort so that bipartite == 1 is on top.
    nodes = nodes[order(-sapply(nodes, function(x) x[['bipartite']]))] 
    stop = length(Filter(function(x) all((x <- x$bipartite == 1)), nodes))
    #calculate the number of nodes with bipartite == 1
    bipartite_num = sum(as.numeric(unlist(nodes)[grepl(pattern='bipartite', names(unlist(nodes)))]))
    bipartite_trans_matrix = matrix
    #assign node pairs in the same set with values of 0
    bipartite_trans_matrix[1:bipartite_num, 1:bipartite_num] <- NA
    bipartite_trans_matrix[(bipartite_num+1):length(nodes), (bipartite_num+1):length(nodes)] <- NA
  } else {
    stop = length(nodes)
  }
  
  for (i in seq(1, stop)) {
    NodesInMatrix = NodesWithinMatrixThres(nodes[[i]][['label']], thres, matrix) #NodesInMatrix includes POI and centroids
    if(bipartite) {
      CentroidsInMatrix = NodesWithinMatrixThres(nodes[[i]][['label']], thres, bipartite_trans_matrix) #CentroidsInMatrix only includes centroids
      numNodesInMatrix = length(CentroidsInMatrix)  
    } else {
      numNodesInMatrix = length(NodesInMatrix)  
    }
    if (numNodesInMatrix < min) {
      labels = c(labels, nodes[[i]][['label']])
      ndensity = c(ndensity, NA)
    } else {
      if(bipartite) {
        if(directed) {dir = 2} else {dir = 1}
        potential = numNodesInMatrix * (length(NodesInMatrix)+1 - numNodesInMatrix) * dir #plus 1 for including the self node
      } else {
        if(directed) {dir = 1} else {dir = 2}
        potential = numNodesInMatrix * (numNodesInMatrix - 1)/dir
      }
      numEdges = getNumEdgesInMatrix(nodes[[i]][['label']], names(NodesInMatrix), edges, matrix, thres, FALSE)
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
    
    if(bipartite) {
      nameWithinMatrix=names(NodesWithinMatrixThres(edge[['Source']], thres, bipartite_trans_matrix))
    } else {
      nameWithinMatrix=names(NodesWithinMatrixThres(edge[['Source']], thres, matrix))
    }
    
    if (edge[['Target']] %in% nameWithinMatrix) {
      withinwindow = c(withinwindow, 1) 
    } else {
      withinwindow = c(withinwindow, 0) 
    }
  }
  
  edgeWithin = data.frame('Source' = source, 'Target' = target, 'WithinWindow' = withinwindow)
  
  return(list(heat, edgeWithin))
}

# ----- Kfullfillment ------#
Kfullfillment = function(nodes, edges, minK=1, bipartite=FALSE) {
  
  if (bipartite) {
    #if bipartite, filter nodes to those that are in set 1
    nodes2 <- nodes[sapply(nodes, function(node) node[['bipartite']] == 1)]
    #the following codes only loop through nodes in set 1
    result = lapply(nodes2, function(node) {
      return(Kfullfillment_for_one_node(nodes2, edges, node, minK, bipartite))
    })
    
    labels = lapply(nodes2, function(node) {
      return(node[['label']])
    })
    
    k_values = unname(unlist(lapply(result, `[[`, "k")))
    
    # Add the 'k' attribute to each node
    for (i in seq_along(nodes2)) {
      nodes2[[i]]$k <- k_values[i]
    }
    # pre-compute KNN for each node to save running time
    knn_list = lapply(nodes2, function(node) {
      #note that nodes in nearestNeighbors need to be unfiltered! 
      names(nearestNeighbors(nodes, node, node[['k']], bipartite))
    })
    names(knn_list) <- names(nodes2)
    
    #if bipartite, only need to check if Target is a k-nearest neighbor of the 'Source'
    edge_table = lapply(edges, function(edge) {
      source_knn = knn_list[[edge[['Source']]]]
      is_k_nearest_neighbor = as.integer(edge[['Target']] %in% source_knn)
      return(list(Source = edge[['Source']], Target = edge[['Target']], is_k_nearest_neighbor = is_k_nearest_neighbor))
    })
    
  } else {
    #no need for nodes2 and filtering
    result = lapply(nodes, function(node) {
      return(Kfullfillment_for_one_node(nodes, edges, node, minK, bipartite))
    })
    
    labels = lapply(nodes, function(node) {
      return(node[['label']])
    })
    
    k_values = unname(unlist(lapply(result, `[[`, "k")))
    
    # Add the 'k' attribute to each node
    for (i in seq_along(nodes)) {
      nodes[[i]]$k <- k_values[i]
    }
    # pre-compute KNN for each node to save running time
    knn_list = lapply(nodes, function(node) {
      names(nearestNeighbors(nodes, node, node[['k']], bipartite))
    })
    names(knn_list) <- names(nodes)
    
    # The edge_table is created by iterating over each edge in the edges list and 
    # checking if the 'Source' node is a k-nearest neighbor of the 'Target' 
    # node or vice versa. If yes, the KNearestNeighbor value is set to 1; 
    # otherwise, it's set to 0.
    edge_table = lapply(edges, function(edge) {
      source_knn = knn_list[[edge[['Source']]]]
      target_knn = knn_list[[edge[['Target']]]]
      is_k_nearest_neighbor = as.integer(edge[['Target']] %in% source_knn | edge[['Source']] %in% target_knn)
      return(list(Source = edge[['Source']], Target = edge[['Target']], is_k_nearest_neighbor = is_k_nearest_neighbor))
    })
  }
  
  node_table = data.frame('label' = unname(unlist(labels)), 'k' = k_values, 
                          'K_fullfillment' = unname(unlist(lapply(result, `[[`, "kf"))))
  
  edge_table = do.call(rbind.data.frame, edge_table)
  
  return(list(node_table, edge_table))
}
