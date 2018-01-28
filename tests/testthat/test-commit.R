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


test_that("restore data", {
  m <- commit_memory_store()
  c <- commit_restore('a', m, .data = FALSE)

  expect_named(c$objects, 'x')
  expect_named(c$tags, 'x')
  expect_true(is.na(c$objects$x))
  expect_equal(c$tags$x, list())

  c <- commit_restore_data(c, m)

  expect_named(c$objects, 'x')
  expect_named(c$tags, 'x')
  expect_equal(c$objects$x, 1)
  expect_named(c$tags$x, c('class', 'time'), ignore.order = TRUE)
  expect_equal(c$tags$x$class, 'numeric')
})


test_that("clean-up works for edge cases", {
  # empty symbol
  expect_equal(cleanup_object(bquote()), bquote())
})
