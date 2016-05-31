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


test_that("restoring by", {
  st <- helper_new_storage()
  for (i in 1:10)
    store_object(st, paste0('abcdef', i), i, list(a = i, b = i+10))
  
  expect_equal(restore_objects_by(st, a == 1), list(abcdef1 = 1))
  expect_equal(restore_objects_by(st, a > 8), list(abcdef10 = 10, abcdef9 = 9))

  expect_equal(restore_tags_by(st, a == 1), list(abcdef1 = list(a = 1, b =11)))
  expect_equal(restore_tags_by(st, a > 8), list(abcdef10 = list(a = 10, b = 20),
                                                abcdef9  = list(a = 9, b = 19)))
})


test_that("restore tags do not exist", {
  st <- helper_new_storage()
  store_object(st, 'abcdef', 1, list(a = 1))
  
  expect_equivalent(restore_objects_by(st, b == 1), list())
})


test_that("errors", {
  st <- helper_new_storage()
  id <- 'abcdef'
  er <- "id 'abcdef' not found in storage"
  expect_error(restore_object(st, id), er)
  expect_error(restore_tags(st, id), er)
})

