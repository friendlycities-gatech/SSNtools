context("run SSNHotSpot with packaged dataset user_nodes and user_edges")

data("example_nodes")
data("example_edges")

nodes <- processNode(example_nodes, 'label', 'lon', 'lat')
edges <- processEdge(example_edges, 'Source', 'Target')

result <- edgeScanRadius(nodes, edges, 500)
result2 <- NDScanRadius(nodes, edges, 500)

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