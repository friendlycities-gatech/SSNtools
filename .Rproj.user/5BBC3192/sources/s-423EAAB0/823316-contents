context("run SSNtools with packaged dataset user_nodes and user_edges")

data("NYCMafiaNodes")
data("NYCMafiaEdges")

data("MafiaNodes")
data("MafiaEdges")

test_that("MafiaNodes loaded successfully", {
  expect_equal(nrow(MafiaNodes), 680)
})

test_that("MafiaEdges loaded successfully", {
  expect_equal(nrow(MafiaEdges), 5398)
})

nodes <- processNode(NYCMafiaNodes, 'label', 'lon', 'lat')
edges <- processEdge(NYCMafiaEdges, 'Source', 'Target')

result <- edgeScanRadius(nodes, edges, 500)
result2 <- NDScanRadius(nodes, edges, 500)
result3 <- NDScanKNearest(nodes, edges, 10)
result4 = NDScanManhattan(nodes, edges, 500)
result5 = edgeScanKNearest(nodes, edges, 10)
result6 = edgeScanManhattan(nodes, edges, 500)

test_that("the number rows in edgeScanRadius outout is the same as the length of nodes", {
  expect_equal(length(nodes),nrow(result))
})

test_that("the total heat in edgeScanRadius outout is the same as 366", {
  expect_equal(sum(result$heat), 366)
})

test_that("the number rows in NDScanRadius outout is the same as the length of nodes", {
  expect_equal(length(nodes),nrow(result2))
})

test_that("the number rows in NDScanRadius outout is the same as 14", {
  expect_equal(round(sum(result2$heat), 0), 14)
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



