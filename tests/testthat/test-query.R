context("query")

test_that("reducing by class", {
  s <- graph_to_steps(sample_graph())
  d <- lazyeval::as.lazy_dots(list(lazyeval::lazy(inherits())))
  m <- storage::memory()
  os_write(m, )

  reduce_steps(s, )
})

