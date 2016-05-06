context("fs")


test_that("creating storage and adding object", {
  path <- file.path(tempdir(), 'storage')
  on.exit(unlink(path, recursive = TRUE, force = TRUE), add = TRUE)
  
  st <- storage(path, .create = T)
  expect_true(file.exists(path))

  id <- store_object(st, 'abcdef', 1)
  pt <- file.path(path, 'ab', 'cd', id)
  expect_true(file.exists(paste0(pt, '.rds')))
  expect_true(file.exists(paste0(pt, '_tags.rds')))
})


test_that("restoring objects", {
  st <- helper_new_storage()
  id <- 'abcdef'
  store_object(st, id, 1, list(a = 1))
  expect_equal(restore_object(st, id), 1)
  expect_equal(restore_tags(st, id), list(a = 1))
})


test_that("errors", {
  st <- helper_new_storage()
  id <- 'abcdef'
  er <- "id 'abcdef' not found in storage"
  expect_error(restore_object(st, id), er)
  expect_error(restore_tags(st, id), er)
})
