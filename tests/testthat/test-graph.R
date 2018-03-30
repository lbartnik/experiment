context("graph")

test_that("root can be found", {
  # in a graph with a single root node
  g <- sample_graph()

  # root is identified
  expect_equal(find_root_id(g), 'a')
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
  expect_named(children, c('a', 'b', 'c'))
  expect_equivalent(children, c('b', 'c', 'd'))
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


test_that("graph subset", {
  g <- sample_graph()
  h <- graph_subset(g, 'path', 'd', 'root')

  expect_true(is_graph(h))
  expect_named(h, c('d', 'c', 'b', 'a'))
  expect_true(all(vapply(h, class, character(1)) == 'commit'))
  expect_true(is_path(h))
})

test_that("find repeats", {
  # a: x <- 1
  # b: y <- 2L
  # c: x <- as.character(x+y)
  # d: z <- as.numeric(x)
  # e: w <- x + 1
  # f: x <- 100L
  g <- graph_subset(expanded_graph(), 'path', 'f', 'root')

  r <- graph_find_repeats(g)
  expect_named(r, "c::r")
  expect_equal(first(r), 'u')
})

test_that("graph to tree", {
  t <- graph_to_tree(sample_graph())

  expect_s3_class(t, 'commit')
  expect_named(t$children, 'b')

  b <- t$children$b
  expect_s3_class(b, 'commit')
  expect_named(b$children, 'c')

  c <- b$children$c
  expect_s3_class(c, 'commit')
  expect_named(c$children, 'd')

  d <- c$children$d
  expect_s3_class(d, 'commit')
  expect_length(d$children, 0)
})



test_that("convert commit", {
  g <- sample_graph()
  r <- commit_to_steps(g$b, c('x', 'y'))

  expect_named(r, c('steps', 'links'))
  expect_length(r$steps, 2)
  expect_length(r$links, 1)

  expect_equal(nth(r$steps, 1)$name, 'x')
  expect_equal(nth(r$steps, 2)$name, 'y')

  expect_named(nth(r$steps, 1),
               c('type', 'name', 'desc', 'id', 'expr', 'commit_id', 'object_id', 'time',
                 'comment'),
               ignore.order = TRUE)

  type <- unique(vapply(r$steps, `[[`, character(1), i = 'type'))
  expect_equal(type, 'object')

  commit_id <- unique(vapply(r$steps, `[[`, character(1), i = 'commit_id'))
  expect_equal(commit_id, 'b')

  link <- nth(r$links, 1)
  expect_named(link, c('source', 'target'))
  expect_equal(link$source, 'p')
  expect_equal(link$target, 'q')
})


test_that("convert graph", {
  g <- sample_graph()
  g <- assign_children(g, find_root_id(g), 1)

  s <- graph_to_steps(g)
  expect_named(s, c('steps', 'links'))
  expect_length(s$steps, 4)
  expect_length(s$links, 3)

  nms <- vapply(s$steps, `[[`, character(1), i = 'name')
  expect_equal(nms, c('x', 'y', 'x', 'z'))

  expect_connected(s, 'p', 'q')
  expect_connected(s, 'q', 'r')
})


test_that("unique step ids", {
  # when two commits contain object of the same id
  m <- commit_memory_store()
  write_commit(m, commit(list(x = 'r'), bquote(), 'a', 'e'))

  # extracting steps
  s <- graph_to_steps(graph(m))

  # every step has an unique id
  ids <- vapply(s$steps, `[[`, character(1), i = 'id')
  expect_length(unique(ids), length(ids))
})



test_that("objects have descriptions", {
  m <- commit_memory_store()
  s <- graph_to_steps(graph(m, .data = TRUE))

  expect_length(s$steps, 4)
  expect_equal(vapply(s$steps, `[[`, character(1), i = 'desc'),
               c('numeric', 'integer', 'character', 'numeric'))
})


test_that("descriptions can be added later", {
  m <- commit_memory_store()
  s <- graph_to_steps(graph(m, .data = FALSE))

  expect_length(s$steps, 4)
  expect_equal(vapply(s$steps, `[[`, character(1), i = 'desc'),
               rep(NA_character_, 4))

  r <- read_objects(s, m)
  expect_equal(vapply(r$steps, `[[`, character(1), i = 'desc'),
               c('numeric', 'integer', 'character', 'numeric'))
})


test_that("finding step by id", {
  s <- sample_steps()
  t <- step_by(s, object_id = 'r')

  expect_equal(t$object_id, 'r')
  expect_equal(t$commit_id, 'c')
  expect_equal(t$name, 'x')
})
