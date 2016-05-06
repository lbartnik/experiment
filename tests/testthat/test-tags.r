context("tags")

test_that("auto tags are generated correctly", {
  obj <- lm(Sepal.Length ~ Species, iris)
  tgs <- auto_tags(obj, environment())
  
  expect_named(tgs, c('class', 'time', 'formula', 'data'))
  expect_equal(tgs$class, "lm")
  expect_equal(tgs$formula, "Sepal.Length ~ Species")
  expect_equal(tgs$data, "iris")
})
