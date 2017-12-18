context("graph")

test_that("root can be found", {
  # in a graph with a single root node
  g <- sample_graph()

  # root is identified
  expect_equal(find_root_id(g), 'c')
})

test_that("only one root allowed", {
  g <- sample_graph()
  g$b$parent <- NA

  expect_error(find_root_id(g))
})

test_that("assign children", {
  g <- sample_graph()
  g <- assign_children(g, find_root_id(g), 1)

  children <- unlist(lapply(g, `[[`, i = 'children'))
  expect_named(children, c('b', 'c'))
  expect_equivalent(children, c('a', 'b'))
})


test_that("filter", {
  g <- sample_graph()
  expect_equal(introduced_in(g, 'b'), 'y')
  expect_equal(introduced_in(g, 'c'), 'x')
})

test_that("filter on plots", {
  g <- structure(list(a = list(parent = 'b',
                               object_ids = list(.plot = 1)),
                      b = list(parent = NA,
                               object_ids = list())))
  expect_equal(introduced_in(g, 'a'), '.plot')
})


test_that("convert commit", {
  g <- sample_graph()
  r <- commit_to_steps(g$a, c('x', 'y'))

  expect_named(r, c('steps', 'links'))
  expect_length(r$steps, 2)
  expect_length(r$links, 1)

  expect_equal(nth(r$steps, 1)$name, 'x')
  expect_equal(nth(r$steps, 2)$name, 'y')

  type <- unique(vapply(r$steps, `[[`, character(1), i = 'type'))
  expect_equal(type, 'object')

  link <- nth(r$links, 1)
  expect_named(link, c('source', 'target'))
  expect_equal(link$source, 'r')
  expect_equal(link$target, 'q')
})


test_that("convert graph", {
  g <- sample_graph()
  g <- assign_children(g, find_root_id(g), 1)

  s <- graph_to_steps(g)
  expect_named(s, c('steps', 'links'))
  expect_length(s$steps, 3)
  expect_length(s$links, 2)

  nms <- vapply(s$steps, `[[`, character(1), i = 'name')
  expect_equal(nms, c('x', 'y', 'x'))

  expect_exists(list(source = 'p', target = 'q'), s$links)
  expect_exists(list(source = 'q', target = 'r'), s$links)
})
