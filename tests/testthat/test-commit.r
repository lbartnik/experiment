context("commit")

test_that("hash values are assigned inside environment", {
  e <- as.environment(list(a = 1, b = list(x = 2), c = data.frame(d = 3, e = 5)))
  update_with_hash(e)
  eapply(e, function(x) expect_true(hash_attribute_name %in% names(attributes(x))))
})


test_that("creating commit", {
  e   <- create_sample_env()
  p   <- empty_commit() ; p$hash <- 'xxx'
  cmt <- commit_from(e, parent = p, bquote(x))
  expect_named(cmt, c('objects', 'history', 'hash', 'parent'))
  expect_equal(cmt$parent, 'xxx')
  expect_equal(cmt$history, bquote(x))
  
  obj <- cmt$objects
  expect_named(obj, c('name', 'hash'))
  expect_equal(obj$name, c('a', 'b'))
  expect_equal(obj$hash, c('hs12', 'ab34'))
})


test_that("creating commit fails", {
  e <- create_sample_env()
  attr(e$a, hash_attribute_name) <- NULL
  expect_error(commit_from(e))
})


test_that("commits are equal", {
  e <- create_sample_env()
  p1 <- empty_commit() ; p1$hash <- 'xxx'
  p2 <- empty_commit() ; p2$hash <- 'xxx'
  
  c1 <- commit_from(e, parent = p1, bquote(x))
  c2 <- commit_from(e, parent = p2, bquote(y))
  
  expect_true(commits_equal(c1, c2))
})


test_that("empty commit compares", {
  e <- create_sample_env()
  cmt <- commit_from(e, parent = empty_commit(), bquote(x))
  expect_false(commits_equal(cmt, empty_commit()))
})
