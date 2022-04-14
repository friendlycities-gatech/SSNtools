#------------------------------------------#
#-----------Utility Methods ---------------#
#------------------------------------------#

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
#Get a certain number of the nearest neighbor hodes to a source node (can suggest increasing the number if numNeighbors > number)
nearestNeighbors = function(nodes, source, number, bipartite) {
  neighbors = list() #a particular structure to store tibble nodes #dict does not accept tibble as the key
  distance_dict = {}
  numNeighbors = 0
  
  for (node in nodes) {
    if(is.null(node[['bipartite']])) {z = 100} else {z = node[['bipartite']]}
    if((bipartite & z == 0) | !bipartite)  {
      label = node[['label']]
      if (node[['label']] != source[['label']]) {
        distance = euclidDistance(source, node)
        if (numNeighbors < number) {
          temp = list()
          temp[[label]] <- node
          neighbors = append(neighbors, temp)
          distance_dict[label] = distance
          numNeighbors = numNeighbors + 1
        } else {
          for (neighbor in neighbors) {
            neigh_label = neighbor[['label']]
            if (distance < distance_dict[neigh_label]) {
              neighbors = neighbors[names(neighbors) != neigh_label]
              temp = list()
              temp[[label]] <- node
              neighbors = append(neighbors, temp)
              distance_dict[label] = distance
              break 
            }
          }
        }
      }
    }
  }
  return(neighbors) #a list of named lists
}

#Get the nearest neighbor to a source node that is not in the visited list
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