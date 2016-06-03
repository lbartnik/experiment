context("extract")


test_that("can find assignment", {
  expect_assignment <- function (name, expr, exp) {
    res <- extract_assignment(name, expr)
    expect_equal(res, exp)
  }

  # first is just a plain x = 1  
  expect_assignment("x", substitute(x %=% 1, list(`%=%` = quote(`=`))),   quote(1))
  expect_assignment("x", quote(x <- 2),  quote(2))
  expect_assignment("x", quote(x <<- 3), quote(3))
  expect_assignment("x", quote({x <- 4 ; z <- 2}), quote(4))
  expect_assignment("x", quote(f(g(x <- 5), z = 2)), quote(5))
  
  expect_assignment("x", quote(x <- iris), quote(iris))
  expect_assignment("x", quote(x <- f(iris, 1)), quote(f(iris, 1)))
  expect_assignment("x", quote(x <- iris[, 1]), quote(iris[, 1]))
  expect_assignment("x", quote(x <- iris %>% select(Species)), quote(iris %>% select(Species)))
  
  expect_assignment("x", quote(x <- iris %>% select(Species) %>% lm(~Species, data = .)),
                         quote(iris %>% select(Species) %>% lm(~Species, data = .)))
})

