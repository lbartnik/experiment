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
  m <- commit_memory_store()
  s <- graph_to_steps(graph(m, .data = FALSE))

  d <- to_lazy_dots(inherits('numeric'))
  t <- reduce_steps(s, d, m)

  expect_length(t$steps, 2)
  expect_length(t$links, 1)
  expect_equal(first(t$links), list(source = 'p', target = 's'))
})


test_that("reducing by name", {
  m <- commit_memory_store()
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


test_that("removing shared parent", {
  s <-
    empty_steps() %>%
    add_step('a', NA_character_) %>%
    add_step('b', 'a') %>%
    add_step('c', 'b') %>%
    add_step('d', 'b')

  expect_length(s$steps, 4)
  expect_length(s$links, 3)

  # after removal
  r <- remove_step(s, 'b')

  expect_length(r$steps, 3)
  expect_length(r$links, 2)
  expect_equal(first(r$steps)$id, 'a')
  expect_equal(first(r$links), list(source = 'a', target = 'c'))
  expect_equal(last(r$links), list(source = 'a', target = 'd'))
})


test_that("replace with virtual root", {
  # this is necessary when we are supposed to remove a node that
  # has no parent and more than one child

  steps <-
    empty_steps() %>%
    add_step('a', NA_character_) %>%
    add_step('b', 'a') %>%
    add_step('c', 'a')

  removed <- remove_step(steps, 'a')

  expect_length(removed$steps, 3)
  expect_length(removed$links, 2)
  expect_equal(first(removed$steps)$name, 'virtual root')
  expect_equal(first(removed$steps)$id, 'a')

  expect_identical(removed$links, steps$links)
})

