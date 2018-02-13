context("JS")

test_that("JS tests pass", {
  skip("fix: browser does not exit")
  expect_true(unittestGadget(browser = FALSE))
})
