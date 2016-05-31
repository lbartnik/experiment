context("commit")


test_that("creating commit", {
  storage_object <- helper_new_storage()

  # sample environment has 2 objects, turn it into a commit;
  # mock restore_object because parent id is not NA
  cmt_id <- with_mock(restore_object = function (...) list(objects = list()),
    {
      e <- create_sample_env()
      store_commit(e, 'parent_id', bquote(x), storage_object)
    })

  expect_equal(count_objects(storage_object), 3)
  
  cmt <- restore_object(storage_object, cmt_id)
  expect_named(cmt, c('objects', 'history'))
  expect_equivalent(cmt$objects, c('a', 'b'))
  expect_equal(cmt$history, bquote(x))
})


test_that("the same commmit is not stored twice", {
  storage_object <- helper_new_storage()
  e <- create_sample_env()
  
  parent_id <- store_commit(e, NA_character_, bquote(x), storage_object)
  child_id_1 <- store_commit(e, parent_id, bquote(x), storage_object)
  
  # reverse the list of objects, should not make any difference
  f <- as.environment(as.list(e)[rev(seq_along(e))])
  child_id_2 <- store_commit(f, parent_id, bquote(x), storage_object)
  
  expect_equal(parent_id, child_id_1)
  expect_equal(child_id_1, child_id_2)
  # (one commit + two objects) * (object file + tag file) = 3 * 2 = 6
  expect_equal(length(list.files(storage_object$path, recursive = TRUE)), 6)
})


test_that("restore commit", {
  storage_object <- helper_new_storage()
  id <- store_commit(create_sample_env(), NA_character_, bquote(x), storage_object)
  restored <- restore_commit(storage_object, id)
  
  expect_s3_class(restored, 'commit')
  expect_named(restored, c('objects', 'history', 'parent', 'time', 'id'))
})

