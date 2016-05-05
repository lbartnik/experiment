context("commit")


test_that("creating commit", {
  storage_path   <- file.path(tempdir(), 'storage')
  storage_object <- storage(storage_path, .create = TRUE)
  on.exit(unlink(storage_path, recursive = TRUE), add = TRUE)
  
  cmt_id <- with_mock(restore_object = function (...) list(objects = list()),
    {
      e <- create_sample_env()
      store_commit(e, 'parent_id', bquote(x), storage_object)
    })

  expect_equal(count_objects(storage_object), 3)
  
  cmt <- restore_object(storage_object, cmt_id)
  expect_named(cmt, c('objects', 'history'))
  expect_named(cmt$objects, c('a', 'b'))
  expect_equal(cmt$history, bquote(x))
})
