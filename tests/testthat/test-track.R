context("track")


test_that("store environment", {
  m <- storage::memory()
  e <- as.environment(list(a = 1, b = 2, c = iris))

  i <- store_environment(m, e, bquote())
  expect_named(i, names(e))
  expect_length(storage::os_list(m), 3)
})


test_that("object is stripped of environments", {
  m <- lm(Sepal.Length ~ Species, iris)
  n <- strip_object(m)

  this_env <- environment()
  expect_identical(attr(m$terms, '.Environment'), this_env)
  expect_identical(attr(n$terms, '.Environment'), emptyenv())
})


test_that("stripping preserves address", {
  skip_if_not_installed("data.table")

  stripped <- strip_object(iris)
  expect_identical(stripped, iris)
  expect_identical(data.table::address(stripped), data.table::address(iris))
})



test_that("recognize stores", {
  st1 <- filled_store(tempdir())
  on.exit(remove_store(st1), add = TRUE)

  ret <- discover_object_store(tempdir())

  expect_length(ret, 1)
  expect_true(dir.exists(as.character(ret)))
  expect_true(storage::is_filesystem_dir(ret))

  # add another store
  st2 <- filled_store(tempdir())
  on.exit(remove_store(st2), add = TRUE)

  ret <- discover_object_store(tempdir())
  expect_length(ret, 2)
  expect_true(all(dir.exists(as.character(ret))))
  expect_true(all(vapply(ret, storage::is_filesystem_dir, logical(1))))

  # empty directory does not change the result
  dir <- file.path(tempdir(), 'xyz')
  dir.create(dir)
  on.exit(unlink(dir), add = TRUE)

  ret <- discover_object_store(tempdir())
  expect_length(ret, 2)
})


test_that("choose store if exists", {
  st <- filled_store(tempdir())
  on.exit(remove_store(st), add = TRUE)

  ret <- prepare_object_store(tempdir())
  expect_s3_class(ret, 'object_store')
  expect_s3_class(ret, 'filesystem')
  expect_equal(as.character(ret), as.character(st))
})


test_that("recognize store in path", {
  st <- filled_store(tempdir())
  on.exit(remove_store(st), add = TRUE)

  ret <- prepare_object_store(as.character(st))
  expect_equal(as.character(ret), as.character(st))
})


test_that("recognize an empty dir as a store", {
  st <- empty_store()
  on.exit(remove_store(st))

  ret <- discover_object_store(as.character(st))
  expect_length(ret, 1)
  expect_equal(ret, as.character(st))
})


test_that("do not choose if more than one", {
  st1 <- filled_store(tempdir())
  on.exit(remove_store(st1), add = TRUE)

  st2 <- filled_store(tempdir())
  on.exit(remove_store(st2), add = TRUE)

  # errors out and asks user to make the choice
  expect_error(ret <- prepare_object_store(tempdir(), FALSE))
})


test_that("create if top dir does not exist", {
  parent_path <- file.path(tempdir(), 'test-parent')
  on.exit(unlink(parent_path, recursive = TRUE, force = TRUE), add = TRUE)

  expect_true(dir.create(parent_path))
  store_path <- file.path(parent_path, 'top-level-store-dir')
  expect_false(dir.exists(store_path))

  expect_warning(ret <- prepare_object_store(store_path, FALSE))
  expect_s3_class(ret, 'filesystem')
  expect_equal(as.character(ret), store_path)
  expect_true(dir.exists(store_path))
})


test_that("reattach", {
  state <- empty_state()
  store <- commit_filesystem_store()
  env   <- new.env()

  reattach_to_store(state, store, env, "abort", TRUE)
  expect_length(env, 3)
  expect_named(env, c("x", "y", "z"), ignore.order = TRUE)
})


test_that("reattach to empty store", {
  state <- empty_state()
  store <- empty_store()
  env   <- new.env()

  reattach_to_store(state, store, env, "abort", TRUE)
  expect_length(env, 0)
  expect_identical(state$stash, store)
})


test_that("reattach to non-empty, overwrite", {
  state <- empty_state()
  store <- commit_filesystem_store()
  env   <- as.environment(list(a = 1))

  expect_error(reattach_to_store(state, store, env, "abort", TRUE))

  expect_warning(reattach_to_store(state, store, env, "overwrite", TRUE))
  expect_length(env, 3)
  expect_named(env, c("x", "y", "z"), ignore.order = TRUE)
  expect_equal(state$last_commit$id, 'd')
})


test_that("reattach with merge", {
  state <- empty_state()
  store <- commit_filesystem_store()
  env   <- as.environment(list(a = 1))

  expect_warning(reattach_to_store(state, store, env, "merge", TRUE))
  expect_length(env, 4)
  expect_named(env, c("a", "x", "y", "z"), ignore.order = TRUE)
  expect_false(identical(state$last_commit$id, 'd'))
  expect_equal(state$last_commit$parent, 'd')
})


test_that("reattach with choice", {
  state <- empty_state()
  store <- commit_filesystem_store()
  commit_store(commit(list(x = 2), bquote(), 'a', 'e', list(x = 't')), store)
  env   <- new.env()

  mockery::stub(reattach_to_store, 'showChoiceDialog', 'e')
  capture_output(reattach_to_store(state, store, env, "abort", FALSE))

  expect_length(env, 1)
  expect_named(env, "x")
  expect_equal(state$last_commit$id, 'e')
})


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
  expect_warning(modelling(overwrite = TRUE))

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
