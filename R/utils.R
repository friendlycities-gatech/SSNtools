#------------------------------------------#
#-----------Utility Methods ---------------#
#------------------------------------------#

#Helper function to convert processNode from for loop to lapply
processNodeHelper = function(row, bipartite_name) {
  temp <- list()
  label <- as.character(row$label)
  
  if (is.na(bipartite_name)) {
    node <- list('label' = label, 'lon' = row$lon, 'lat' = row$lat)
  } else {
    if (is.null(row$bipartite)) {
      stop('Node bipartite value is not available. Please check if the node table contains bipartite values and if the name of the bipartite column is provided in the processNode function')
    }
    node <- list('label' = label, 'lon' = row$lon, 'lat' = row$lat, 'bipartite' = row$bipartite)
  }
  
  return(setNames(list(node), label))
}

#Helper function to convert processNode from for loop to lapply
processEdgeHelper <- function(row, source_name, target_name, weight_name) {
  if (is.na(weight_name)) {
    edge <- list('Source' = as.character(row[[source_name]]), 'Target' = as.character(row[[target_name]]))
  } else {
    if (is.null(row[[weight_name]])) {
      stop('Edge weight is not available. Please check if edge table contains a weight column and if the name of the weight column is provided in the processEdge function')
    }
    edge <- list('Source' = as.character(row[[source_name]]), 'Target' = as.character(row[[target_name]]), 'Weight' = row[[weight_name]])
  }
  return(edge)
}

#Get number of nodes within manhattan distance of source node
numberNodesWithinManhattanDistance = function(nodes, source, distance, bipartite) {
  retValue = 0
  for (node in nodes) {
    dist = ManhattanDistance(source, node)
    if(bipartite) {
      if(node[['bipartite']] == 0) {
        if (dist <= distance) {retValue = retValue + 1} 
      }
    } else {
      if (dist <= distance) {retValue = retValue + 1}
    }
  }
  return (retValue)
}

#Get number of edges within manhattan distance of source node
numberEdgesWithinManhattanDistance = function(nodes, edges, source, distance, weighted) {
  retValue = 0
  for (edge in edges) {
    dist1 = ManhattanDistance(source, nodes[[edge[['Source']]]])
    dist2 = ManhattanDistance(source, nodes[[edge[['Target']]]])
    if (dist1 <= distance & dist2 <= distance) {
      if(weighted) {
        if(is.null(edge[['Weight']])) {
          stop('Edge weight is not available. Please check if edge table contains a weight column and if the name of the weight column is provided in the processEdge function')
        }
        retValue = retValue + edge[['Weight']]
      } else {
        retValue = retValue + 1
      }
    }
  }
  return(retValue)
}

#Get the list of nodes within manhattan distance of source node
getNodesInManhattanDistance = function (nodes, source, distance) {
  retNodes = c()
  for (node in nodes) {
    dist = ManhattanDistance(source, node)
    if (dist <= distance) {
      retNodes = append(retNodes, node)
    }
  }
  return (retNodes)
}

#Get the list of edges within manhattan distance of source node
# ---- UNUSED ------ #
getEdgesInManhattanDistance = function(nodes, edges, source, distance) {
  retList = c()
  for (edge in edges) {
    if (testEdgeInRange(nodes, edge, source, distance)) {
      retList = append(retList, edge)
    }
  }
  return (retList)
}

#Determines if two nodes are equal based on having same coordinates
nodeEquals = function(node1, node2) {
  if ((node1[['lat']] == node2[['lat']]) & (node1[['lon']] == node2[['lon']])) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

#Determines if two edges are equal based on consisting of same two nodes in either direction
edgeEquals = function(edge1, edge2, nodes) {
  if ((nodeEquals(nodes[[edge1[['Source']]]], nodes[[edge2[['Source']]]]) & nodeEquals(nodes[[edge1[['Target']]]], nodes[[edge2[['Target']]]])) |
      (nodeEquals(nodes[[edge1[['Source']]]], nodes[[edge2[['Target']]]]) & nodeEquals(nodes[[edge1[['Target']]]], nodes[[edge2[['Source']]]]))) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

#Get Manhattan distance between two nodes
ManhattanDistance = function(node1, node2) {
  return (abs(node1[['lat']] - node2[['lat']]) + abs(node1[['lon']] - node2[['lon']]))
}

#Get Euclidean distance between two nodes
euclidDistance = function(node1, node2) {
  return (sqrt((node1[['lat']] - node2[['lat']]) ** 2 + (node1[['lon']]-node2[['lon']]) ** 2))
}

#Calculate midpoint of an edge
#edge is a 1*2 tibble that has source and target as node names. nodes is the full node list.
calculateMidpoint = function(edge, nodes) {
  source = edge[['Source']]
  target = edge[['Target']]
  x = (max(nodes[[source]][['lat']], nodes[[target]][['lat']]) + min(nodes[[source]][['lat']], nodes[[target]][['lat']])) / 2
  y = (max(nodes[[source]][['lon']], nodes[[target]][['lon']]) + min(nodes[[source]][['lon']], nodes[[target]][['lon']])) / 2
  return(x,y)
}

#for KNN only
#Get a certain number of the nearest neighbor nodes to a source node (can suggest increasing the number if numNeighbors > number)
nearestNeighbors = function(nodes, source, number, bipartite) {
  if(bipartite) {
    #only search neighbors in nodes where bipartite == 0
    nodes2 = nodes[sapply(nodes, function(node) node[['bipartite']] == 0)]
  } else {
    nodes2 = nodes
  }
  
  distances = lapply(nodes2, function(node) {
    if(node[['label']] != source[['label']]) {
      return(euclidDistance(source, node))
    } else {
      return(NULL)
    }
  })
  distances = Filter(Negate(is.null), distances)
  
  order_idx = order(unlist(distances), decreasing = FALSE)
  neighbors = names(distances[order_idx[1:number]])
  neighbors_lst = sapply(neighbors, function(name) {nodes[[name]]}, simplify = FALSE)
  return(neighbors_lst)
}
# nearestNeighbors = function(nodes, source, number, bipartite) {
#   neighbors = list() #a particular structure to store tibble nodes #dict does not accept tibble as the key
#   distance_dict = {}
#   numNeighbors = 0
#   
#   for (node in nodes) {
#     if(is.null(node[['bipartite']])) {z = 100} else {z = node[['bipartite']]}
#     if((bipartite & z == 0) | !bipartite)  {
#       label = node[['label']]
#       if (node[['label']] != source[['label']]) {
#         distance = euclidDistance(source, node)
#         if (numNeighbors < number) {
#           temp = list()
#           temp[[label]] <- node
#           neighbors = append(neighbors, temp)
#           distance_dict[label] = distance
#           numNeighbors = numNeighbors + 1
#         } else {
#           for (neighbor in neighbors) {
#             neigh_label = neighbor[['label']]
#             if (distance < distance_dict[neigh_label]) {
#               neighbors = neighbors[names(neighbors) != neigh_label]
#               temp = list()
#               temp[[label]] <- node
#               neighbors = append(neighbors, temp)
#               distance_dict[label] = distance
#               break 
#             }
#           }
#         }
#       }
#     }
#   }
#   return(neighbors) #a list of named lists
# }

#Get the nearest neighbor to a source node that is not in the visited list
# --- UNUSED ------#
nearestUnvisitedNeighbor = function(nodes, visited, source, bipartite) {
  minNode = NULL
  minDistance = -1
  if(bipartite) {
    for (node in nodes) {
      if (node[['label']] != source[['label']] & (!node[['label']] %in% names(visited)) & node[['bipartite']] == 1) {
        distance = euclidDistance(source, node)
        if (minDistance == -1 | distance < minDistance) {
          minDistance = distance
          minNode = node
        }
      }
    }
  } else {
    for (node in nodes) {
      if (node[['label']] != source[['label']] & (!node[['label']] %in% names(visited))) {
        distance = euclidDistance(source, node)
        if (minDistance == -1 | distance < minDistance) {
          minDistance = distance
          minNode = node
        }
      }
    }
  }
  return(minNode)
}

#Get all the nodes within a certain radius of a source node
# ---- UNUSED -----#
nodesWithinRadius = function(nodes, source, radius) {
  retNodes = list()
  for (node in nodes) {
    distance = euclidDistance(source, node)
    if (distance <= radius) {
      label = node[['label']]
      temp = list()
      temp[[label]] <- node
      retNodes = append(retNodes, temp)
    }
  }
  return(retNodes)
}

numberNodesWithinRadius = function(nodes, source, radius, bipartite) {
  numNodes = 0
  for (node in nodes) {
    distance = euclidDistance(source, node)
    #it only counts target nodes if bipartite = TRUE
    if(bipartite) {
      if(node[['bipartite']] == 0) {
        if (distance <= radius) {numNodes = numNodes + 1} 
      }
    } else {
      if (distance <= radius) {numNodes = numNodes + 1} 
    }
  }
  return (numNodes)
}

#Tests if an edge is fully within a radius of a given node
testEdgeInRange = function(nodes, edge, node, radius) {
  edgeSource = edge[['Source']]
  edgeTarget = edge[['Target']]
  if (is.null(nodes[[edgeSource]]) | is.null(nodes[[edgeTarget]])) {
    stop('Cannot find edge source or target in the node table. Please filter your edge table to contain edges that have corresponding nodes in the node table, or double check whether your node label column has the same values as edge source and target columns.')
  } else {
    if (euclidDistance(node, nodes[[edgeSource]]) <= radius & euclidDistance(node, nodes[[edgeTarget]]) <= radius) {
      return (TRUE)
    } else {
      return (FALSE)
    }
  }
}

testEdgeInRangeMatrix = function(edge, matrix, thres) {
  edgeSource = edge[['Source']]
  edgeTarget = edge[['Target']]
  if (!edgeSource %in% rownames(matrix) | !edgeTarget %in% rownames(matrix)) {
    stop('Cannot find edge source or target in the node table. Please filter your edge table to contain edges that have corresponding nodes in the node table, or double check whether your node label column has the same values as edge source and target columns.')
  } else {
    if (!is.na(matrix[edgeSource, edgeTarget]) & matrix[edgeSource, edgeTarget] <= thres) {
      return (TRUE)
    } else {
      return (FALSE)
    }
  }
}

#Gets the number of edges within a radius of a given node
getNumEdgesInRange = function(nodes, edges, node, radius, weighted) {
  retVal = 0 
  for (edge in edges) {
    if (testEdgeInRange(nodes, edge, node, radius)) {
      if(weighted) {
        if(is.null(edge[['Weight']])) {
          stop('Edge weight is not available. Please check if edge table contains a weight column and if the name of the weight column is provided in the processEdge function')
        } else {retVal = retVal + edge[['Weight']]}
      } else {
        retVal = retVal + 1
      }
    }
  }
  return (retVal)
}

#Gets the list of edges within a radius of a given node
getEdgesInRange = function(nodes, edges, node, radius) {
  retList = list()
  for (edge in edges) {
    if (testEdgeInRange(nodes, edge, node, radius)) {
      retList[[length(retList) + 1]] = edge
    }
  }
  return (retList)
}

#Tests if there is an edge between two given nodes
edgeExists = function(edges, nodes, node1, node2) {
  edge2 = list('Source' = node1[['label']], 'Target' = node2[['label']])
  for (edge in edges) {
    if (edgeEquals(edge, edge2, nodes)) {
      return (TRUE)
    } else {
      return (FALSE)
    }
  }
}

#Generates the adjacency matrix for a graph of nodes and edges
getAdjacencyMatrix = function(nodes, edges) {
  dim = length(nodes)
  adj = matrix(c(0), nrow=dim, ncol=dim)
  i = 0
  for (node in nodes) {
    j = 0
    for (node2 in nodes) {
      if (edgeExists(edges, nodes, node, node2)) {
        adj[i][j] = 1
        adj[j][i] = 1
      }
      j = j + 1 
    }
  }
  for (i in 1:dim) {
    adj[i][i] = 0
  }
  return (adj)
}

multiply = function(A, B, C) {
  V = nrow(A)
  for (i in 1:V) {
    for (j in 1:V) {
      C[i][j] = 0
      for (k in 1:V) {
        C[i][j] = C[i][j] + A[i][k] * B[k][j]
      }
    }
  }
}

# Utility function to calculate trace of a matrix (sum of diagnonal elements) 
getTrace = function(graph) {
  V = nrow(graph)
  trace = 0
  for (i in 1:V) {
    trace = trace + graph[i][i]
  }
  return (trace)
}

# Utility function for calculating number of triangles in graph  
triangleInGraph = function(graph) {
  V = nrow(graph)
  aux2 = matrix(c(0), nrow=V, ncol=V)
  aux3 = matrix(c(0), nrow=V, ncol=V)
  
  # aux2 is graph^2 now printMatrix(aux2)  
  multiply(graph, graph, aux2)
  
  # after this multiplication aux3 is  
  # graph^3 printMatrix(aux3) 
  multiply(graph, aux2, aux3)  
  
  trace = getTrace(aux3)  
  return (floor(trace / 6))
}

#return a matrix. If the network is bipartite, assign value 0 to pairs that are not supposed to have connections, i.e., in the same category
NodesWithinMatrixThres = function(node_label, thres, matrix){
  #return matrix of nodes with only node pairs within distance threshold, excluding same nodes.  
  return(matrix[node_label,][matrix[node_label,]<thres & !is.na(matrix[node_label,])]) #matrix[node_label,]!=0
}

getNumEdgesInMatrix = function(node_label, names, edges, matrix, thres, weighted) {
  retVal = 0
  for (edge in edges) {
    if(edge[['Source']] %in% c(names, node_label) & edge[['Target']] %in% c(names, node_label) & testEdgeInRangeMatrix(edge, matrix, thres)) {
      if(weighted) {
        if(is.null(edge[['Weight']])) {
          stop('Edge weight is not available. Please check if edge table contains a weight column and if the name of the weight column is provided in the processEdge function')
        } else {retVal = retVal + edge[['Weight']]}
      } else {
        retVal = retVal + 1
      }
    }
  }
  return (retVal)
}

#helper function for Kfullfillment()
Kfullfillment_for_one_node = function(node, minK=1, bipartite=FALSE) {
  if (node[['K']] >= minK) {
    #calculate k-fullfillment: number of nodes connected that are also the k-nearest neighbor vs. degree
    kf = length(intersect(node[['connected_nodes']], node[['Knn']]))/node[['K']]
  } else {
    kf = NA
  }
  return(kf)
}

#K (node's degree), connected nodes, and Knn to a node's attributes; 
# connected nodes is NULL if the node is isolated. The attribute may be hidden if the node is printed, but should return NULL when called node[['connected_nodes']]
Add_K_connected_nodes_knn_to_node = function(nodes, edges, node, bipartite) {
  if(bipartite) {
    matching_edges = lapply(edges, function(edge) {
      if(edge[['Source']] == node[['label']]) {
        return(edge[c(1,2)])
      } else {
        return(NULL)
      }
    })
  } else {
    matching_edges = lapply(edges, function(edge) {
      if(edge[['Source']] == node[['label']] | edge[['Target']] == node[['label']]) {
        return(edge[c(1,2)])
      } else {
        return(NULL)
      }
    })
  }
  
  #calculate degree
  matching_edges <- Filter(Negate(is.null), matching_edges)
  degree = length(matching_edges)
  
  #find all nodes connected
  connected_nodes = unname(unlist(matching_edges))
  connected_nodes = connected_nodes[connected_nodes != node[['label']]]
  
  #find k-nearest neighbors
  knn = names(nearestNeighbors(nodes, node, degree, bipartite))
  
  node[['K']] <- degree
  node[['Knn']] <- knn
  node[['connected_nodes']] <- connected_nodes
  
  return(node)
}

# calculate local flattening ratio for one node
LocalFlatteningRatio_for_one_node = function(nodes, node, minK, bipartite) {
  if (node[['K']] >= minK) {
    #return a list of distance between node and its K-nearest neighbors 
    Knn_dist_lst = lapply(node[['Knn']], function(node_label) {
      return(euclidDistance(node, nodes[[node_label]]))
    })
    #calculate the node's minimized/optimized distance 
    d_opt = sum(unlist(Knn_dist_lst))
    #return a list of distance between node and its K-nearest neighbors 
    connected_nodes_dist_list = lapply(node[['connected_nodes']], function(node_label) {
      return(euclidDistance(node, nodes[[node_label]]))
    })
    #calculate the node's total actual distance of its connections 
    d_act = sum(unlist(connected_nodes_dist_list))
    fr = d_opt/d_act
  } else {
    fr = NA
  }
  return(fr)
}

# helper function for calculating global flattening ratio; sum of distance in G_bar
G_bar_sum_distances <- function(node_orders, nodes_w_k_knn, distance_matrix, degree_constraint_matrix) {
  degree_count <- sapply(nodes_w_k_knn, function(x) 0)
  names(degree_count) <- node_orders
  
  nodes_labels <- names(nodes_w_k_knn)
  n <- length(nodes_labels)
  connection_counted <- matrix(FALSE, nrow = n, ncol = n, dimnames = list(nodes_labels, nodes_labels))
  
  total_distance <- 0
  
  for (i in seq_along(node_orders)) {
    node <- node_orders[i]
    neighbors <- nodes_w_k_knn[[node]]$Knn
    
    for (neighbor in neighbors) {
      if (!connection_counted[node, neighbor] && degree_count[node] < degree_constraint_matrix[node] && degree_count[neighbor] < degree_constraint_matrix[neighbor]) {
        total_distance <- total_distance + distance_matrix[node, neighbor]
        degree_count[node] <- degree_count[node] + 1
        degree_count[neighbor] <- degree_count[neighbor] + 1
        connection_counted[node, neighbor] <- TRUE
        connection_counted[neighbor, node] <- TRUE
        #cat("Added distance for", node, neighbor, "in order:", node_orders, "\n")
      }
    }
  }
  return(total_distance)
}

# helper function for calculating global flattening ratio; sum of distance in G
Sum_connected_nodes_distances <- function(nodes_w_k_knn, distance_matrix) {
  # Initialize the sum
  total_distance <- 0
  
  # Iterate through nodes in the nodes_w_k_knn
  for (node in names(nodes_w_k_knn)) {
    # Extract the connected nodes for the current node
    connected_nodes <- nodes_w_k_knn[[node]]$connected_nodes
    
    if(!is.null(connected_nodes)) {
      # Calculate the sum of distances for connected nodes
      node_distance <- sum(sapply(connected_nodes, function(neighbor) {
        # Add the distance if the node and neighbor indices are valid
        # this is to only count distance once for undirected graph
        if (neighbor > node) {
          distance_matrix[node, neighbor]
        } else {
          0
        }
      }))
    } else {
      # if the node is isolated and have no connected nodes 
      node_distance <- 0
    }
    # Add the current node distance to the total distance
    total_distance <- total_distance + node_distance
  }
  
  return(total_distance)
}
