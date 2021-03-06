context("run SSNtools with packaged dataset user_nodes and user_edges")

data("NYCMafiaNodes")
data("NYCMafiaEdges")

data("MafiaNodes")
data("MafiaEdges")

data("DTMafiaNodes")
data("DTMafiaEdges")

data("POINodes")
data('POIEdges')

test_that("MafiaNodes loaded successfully", {
  expect_equal(nrow(MafiaNodes), 680)
})

test_that("MafiaEdges loaded successfully", {
  expect_equal(nrow(MafiaEdges), 2699)
})

test_that("DTMafiaNodes loaded successfully", {
  expect_equal(nrow(DTMafiaNodes), 40)
})

test_that("DTMafiaEdges loaded successfully", {
  expect_equal(nrow(DTMafiaEdges), 147)
})

test_that("POINodes loaded successfully", {
  expect_equal(nrow(POINodes), 1356)
})

test_that("POIEdges loaded successfully", {
  expect_equal(nrow(POIEdges), 7926)
})

nodes <- processNode(NYCMafiaNodes, 'label', 'LonX', 'LatY')
edges <- processEdge(NYCMafiaEdges, 'Source', 'Target')

result <- edgeScanRadius(nodes, edges, 500)[[1]]
result2 <- NDScanRadius(nodes, edges, 500)[[1]]
result3 <- NDScanKNearest(nodes, edges, 10)[[1]]
result4 = NDScanManhattan(nodes, edges, 500)[[1]]
result5 = edgeScanKNearest(nodes, edges, 10)[[1]]
result6 = edgeScanManhattan(nodes, edges, 500)[[1]]

test_that("the number rows in edgeScanRadius outout is the same as the length of nodes", {
  expect_equal(length(nodes),nrow(result))
})

test_that("the total heat in edgeScanRadius outout is the same as 353", {
  expect_equal(sum(result$heat, na.rm = T), 353)
})

test_that("the number rows in NDScanRadius outout is the same as the length of nodes", {
  expect_equal(length(nodes),nrow(result2))
})

test_that("the number rows in NDScanRadius output is the same as 14", {
  expect_equal(round(sum(result2$heat, na.rm = T), 0), 14) 
})

test_that("the number rows in NDScanKNearest outout is the same as the length of nodes", {
  expect_equal(length(nodes),nrow(result3))
})

test_that("the number rows in NDScanManhattan outout is the same as the length of nodes", {
  expect_equal(length(nodes),nrow(result4))
})

test_that("the number rows in edgeScanKNearest outout is the same as the length of nodes", {
  expect_equal(length(nodes),nrow(result5))
})

test_that("the number rows in edgeScanManhattan outout is the same as the length of nodes", {
  expect_equal(length(nodes),nrow(result6))
})

nodes <- processNode(POINodes, 'label', 'LonX', 'LatY', 'Bipartite')
edges <- processEdge(POIEdges, 'Source', 'Target', 'Weight')

result <- edgeScanRadius(nodes, edges, 1000, weighted=TRUE, bipartite=TRUE)
result2 <- NDScanRadius(nodes, edges, 1000, directed=FALSE, bipartite=TRUE)
result3 <- NDScanKNearest(nodes, edges, 3, directed=FALSE, bipartite=TRUE)
result4 <- NDScanManhattan(nodes, edges, 1000, directed=FALSE, bipartite=TRUE)
result5 <- edgeScanKNearest(nodes, edges, 3, weighted=TRUE, bipartite=TRUE)
result6 <- edgeScanManhattan(nodes, edges, 1000, weighted=TRUE, bipartite=TRUE)

test_that("the number of nodes in output is as expected", {
  expect_equal(1045,nrow(result[[1]]))
  expect_equal(1045,nrow(result2[[1]]))
  expect_equal(1045,nrow(result3[[1]]))
  expect_equal(1045,nrow(result4[[1]]))
  expect_equal(1045,nrow(result5[[1]]))
  expect_equal(1045,nrow(result6[[1]]))
})

test_that("the number of edges in output is as expected", {
  expect_equal(length(edges),nrow(result[[2]]))
  expect_equal(length(edges),nrow(result2[[2]]))
  expect_equal(length(edges),nrow(result3[[2]]))
  expect_equal(length(edges),nrow(result4[[2]]))
  expect_equal(length(edges),nrow(result5[[2]]))
  expect_equal(length(edges),nrow(result6[[2]]))
})





