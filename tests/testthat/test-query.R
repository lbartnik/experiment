context("query")

test_that("one-step reduction", {
  # graph structure
  s <- list(
    steps = list(
      list(id = 'a', name = 'a'),
      list(id = 'b', name = 'b'),
      list(id = 'c', name = 'c')
    ),
    links = list(
      list(source = 'a', target = 'b'),
      list(source = 'b', target = 'c')
    )
  )

  # we need storage to read tags
  m <- storage::memory()

  # cut "b" out
  s <- remove_step(`class<-`(s, 'steps'), 'b')

  expect_length(s$steps, 2)
  expect_length(s$links, 1)
  expect_equal(s$links[[1]], list(source = 'a', target = 'c'))
  expect_equal(vapply(s$steps, `[[`, character(1), i = 'id'),
               c('a', 'c'))
})


test_that("reducing by class", {
  m <- sample_memory_store()
  s <- graph_to_steps(graph(m, .data = FALSE))

  d <- to_lazy_dots(inherits('numeric'))
  t <- reduce_steps(s, d, m)

  expect_length(t$steps, 2)
  expect_length(t$links, 1)
  expect_equal(first(t$links), list(source = 'p', target = 's'))
})


test_that("reducing by name", {
  m <- sample_memory_store()
  s <- graph_to_steps(graph(m, .data = FALSE))

  d <- to_lazy_dots(is_named('z'))
  t <- reduce_steps(s, d, m)

  expect_length(t$steps, 1)
  expect_length(t$links, 0)

  d <- to_lazy_dots(is_named('x'))
  t <- reduce_steps(s, d, m)

  expect_length(t$steps, 2)
  expect_length(t$links, 1)
  expect_equal(first(t$links), list(source = 'p', target = 'r'))
})
