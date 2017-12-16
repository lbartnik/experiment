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
  expect_named(prev$objects, c('x', '.plot'), ignore.order = TRUE)

  root <- commit_restore(prev$parent, state$stash)
  expect_identical(root$objects, list(x = 1))
})


test_that("multiple plots", {
  state <- empty_state()

  update_current_commit(state, list(x = 1), NULL, bquote(x <- 1))
  update_current_commit(state, list(x = 2), random_plot(), bquote(plot(1)))
  update_current_commit(state, list(x = 3), random_plot(), bquote(plot(1)))
  update_current_commit(state, list(x = 4), NULL, bquote(x <- 2))

  last <- state$last_commit
  expect_identical(last$objects, list(x = 4))

  prev <- commit_restore(last$parent, state$stash)
  expect_named(prev$objects, c('x', '.plot'), ignore.order = TRUE)
  expect_equal(prev$objects$x, 3)

  prv2 <- commit_restore(prev$parent, state$stash)
  expect_named(prev$objects, c('x', '.plot'), ignore.order = TRUE)
  expect_false(identical(prev$objects$.plot, prv2$objects$.plot))
  expect_equal(prv2$objects$x, 2)

  root <- commit_restore(prv2$parent, state$stash)
  expect_identical(root$objects, list(x = 1))
})


test_that("restoring by commit", {
  # long (full) id
  modelling(overwrite = TRUE)

  id <- '96ea722bf140a98c6854f9532985372a768df257'
  restore(id)
  expect_equal(internal_state$last_commit$id, id)

  # short it
  modelling(overwrite = TRUE)

  restore('96ea722b')
  expect_equal(internal_state$last_commit$id, id)
})


test_that("commit restored correctly", {
  session <- new.env()
  state <- empty_state()
  modelling(TRUE, state)

  restore_commit(state, '96ea722bf140a98c6854f9532985372a768df257', session)

  expect_equal(state$last_commit$id, '96ea722bf140a98c6854f9532985372a768df257')
  expect_named(session, c('iris2', 'x'), ignore.order = TRUE)
  expect_equal(session$iris2, iris)
  expect_s3_class(session$x, 'lm')
})
