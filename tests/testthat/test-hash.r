context("hash")


test_that("shorten hash", {
  expect_equal(shorten("abcdefgh"), "abcdefgh")
  expect_equal(shorten("abcd000000000efgh"), "abcdefgh")
})
