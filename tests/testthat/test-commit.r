context("commit")


test_that("creating commit", {
  e   <- create_sample_env()
  p   <- empty_commit()
  cmt <- create_commit(e, bquote(x))
  expect_named(cmt, c('objects', 'history'))
  expect_equal(cmt$history, bquote(x))
  
  obj <- cmt$objects
  expect_named(obj, c('a', 'b'))
  expect_equal(obj, eapply(e, hash)[c('a', 'b')])
})
