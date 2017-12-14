context("graph")

test_that("root can be found", {
  # in a graph with a single root node
  g <- structure(list(`1` = list(parent = 2), `2` = list(parent = 3), `3` = list(parent = NA)),
                 class = 'graph')
  # root is identified
  expect_equal(find_root_id(g), '3')
})

test_that("only one root", {
  g <- structure(list(`1` = list(parent = 2), `2` = list(parent = NA), `3` = list(parent = NA)),
                 class = 'graph')
  expect_error(find_root_id(g))
})
