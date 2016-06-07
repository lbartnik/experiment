context("commit")


test_that("create commit", {
  with_mock(
    store_object  = mock('id1', 'id2'),
    object_exists = mock(FALSE, cycle = TRUE),
    {
      env <- as.environment(list(a = 1, b = 2))
      cm  <- create_commit(env, null_storage)
      
      expect_s3_class(cm, 'commit')
      expect_named(cm, 'objects')
      expect_named(cm$object, c('id1', 'id2'))
      expect_equivalent(cm$objects, c('a', 'b'))
    }
  )
})


test_that("create commit with object that exists", {
  with_mock(
    store_object  = mock('id'),
    object_exists = mock(TRUE, FALSE),
    {
      env <- as.environment(list(a = 1, b = 2))
      cm  <- create_commit(env, null_storage)

      expect_s3_class(cm, 'commit')
      expect_named(cm, 'objects')
      expect_named(cm$object, c(hash(1), 'id'))
      expect_equivalent(cm$objects, c('a', 'b'))
    }
  ) # with_mock
})


test_that("store commit", {
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
  expect_named(cmt, 'objects')
  expect_equivalent(cmt$objects, c('a', 'b'))
  
  tgs <- restore_tags(storage_object, cmt_id)
  expect_equal(tgs$history, bquote(x))
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
  expect_named(restored, c('objects', 'parent', 'history', 'time', 'id'))
})

