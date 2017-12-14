context("track")


test_that("commit is updated", {
  state <- empty_state()
  env <- as.environment(list(x = 1))
  exp <- substitute(x <- 1)

  update_current_commit(state, env, NULL, exp)

  id <- storage::os_list(state$stash)
  expect_length(id, 2)
  expect_true(storage::compute_id(1) %in% id)
})


test_that("plot is cached", {
  state <- empty_state()

  update_current_commit(state, list(x = 1), NULL, bquote(x <- 1))
  update_current_commit(state, list(x = 1), dummy_plot(), bquote(plot(1)))
  update_current_commit(state, list(x = 2), NULL, bquote(x <- 2))

  last <- state$last_commit
  expect_identical(last$objects, list(x = 2))

  prev <- commit_restore(last$parent, state$stash)
  expect_named(prev$objects, 'plot1')

  root <- commit_restore(prev$parent, state$stash)
  expect_identical(root$objects, list(x = 1))
})


test_that("multiple plots", {
  state <- empty_state()

  update_current_commit(state, list(x = 1), NULL, bquote(x <- 1))
  update_current_commit(state, list(x = 1), random_plot(), bquote(plot(1)))
  update_current_commit(state, list(x = 1), random_plot(), bquote(plot(1)))
  update_current_commit(state, list(x = 1), random_plot(), bquote(plot(1)))
  update_current_commit(state, list(x = 2), NULL, bquote(x <- 2))

  last <- state$last_commit
  expect_identical(last$objects, list(x = 2))

  prev <- commit_restore(last$parent, state$stash)
  expect_named(prev$objects, paste0('plot', 1:3))

  root <- commit_restore(prev$parent, state$stash)
  expect_identical(root$objects, list(x = 1))
})
