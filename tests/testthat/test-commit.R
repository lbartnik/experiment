context("commit")


test_that("commit is stored", {
  m <- storage::memory()
  c <- commit(list(x = 'object-id'), bquote(), 'parent', 'this')
  write_commit(m, c)

  expect_setequal('this', storage::os_list(m))
})


test_that("restore data", {
  m <- commit_memory_store()
  c <- commit_restore('a', m, .data = FALSE)

  expect_length(c$objects, 0)
  expect_named(c$tags, 'x')
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
  expect_equal(strip_object(bquote()), bquote())
})
