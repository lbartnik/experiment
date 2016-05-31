context("interactive")

test_that("all commit are read", {
  s <- helper_new_storage()
  e1 <- create_sample_env()
  e2 <- create_sample_env()
  e3 <- create_sample_env()
  
  e2$x <- 3
  e3$z <- 5
  
  id1 <- store_commit(e1, NA_character_, quote(x), s)
  id2 <- store_commit(e2, NA_character_, quote(x), s)
  id3 <- store_commit(e3, NA_character_, quote(x), s)
  
  x <- restore_all_commits(s)
  x <- x[order(vapply(x, `[[`, numeric(1), 'time'))]
  
  expect_length(x, 3)
  expect_equivalent(vapply(x, class, character(1)), rep('commit', 3))
  
  # ordered by time, ignore names (hash sums)
  expect_equivalent(x[[1]]$objects, c('a', 'b'))
  expect_equivalent(x[[2]]$objects, c('a', 'b', 'x'))
  expect_equivalent(x[[3]]$objects, c('a', 'b', 'z'))
  
  # check names
  expect_named(x, c(id1, id2, id3))
})
