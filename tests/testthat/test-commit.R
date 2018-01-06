context("commit")


test_that("commit is stored", {
  m <- storage::memory()
  c <- commit(list(x = 1), bquote(), 'parent', 'this')
  commit_store(c, m)

  expect_setequal(c('this', storage::compute_id(1)),
                  storage::os_list(m))
})


test_that("store with ids", {
  m <- storage::memory()
  c <- commit(list(x = 1), bquote(), 'parent', 'this',
              object_ids = list(x = 'id'))

  commit_store(c, m)

  expect_setequal(c('this', 'id'), storage::os_list(m))
})


test_that("clean-up works for edge cases", {
  # empty symbol
  expect_equal(cleanup_object(bquote()), bquote())
})
