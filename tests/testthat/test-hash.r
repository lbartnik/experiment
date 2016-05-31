context("hash")


test_that("shorten hash", {
  expect_equal(shorten("abcd"), "abcd")
  expect_equal(shorten("abcdefgh"), "abcdefgh")
  expect_equal(shorten("abcdefgh000000000"), "abcdefgh")
})
