context("query")

test_that("one-step reduction", {

})

test_that("reducing by class", {
  m <- sample_memory_store()
  s <- graph_to_steps(graph(m, .data = FALSE))
  d <- lazyeval::as.lazy_dots(list(lazyeval::lazy(inherits('numeric'))))

  reduce_steps(s, d, m)
})

