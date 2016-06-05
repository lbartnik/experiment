context("extract")

expect_assignment <- function (name, expr, exp) {
  res <- extract_assignment(name, expr)
  expect_equal(res, exp)
}


test_that("can find assignment", {
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


test_that("extraction errors", {
  expect_assignment("x", quote({ x <- 1 ; x <- 2}),  NULL)
  expect_assignment("x", quote({ x <- 1 ; x = 2}),  NULL)

  res <- search_for_assignment("x", quote({ x <- 1 ; x <- 2 ; x = 3}))
  expect_equal(res, list(quote(1), quote(2), quote(3)))
  
})


test_that("user-defined parents are extracted", {
  expect_parents <- function (expr, env, expected) {
    expr    <- substitute(expr)
    parents <- extract_parents(expr, env)
    expect_equal(env, expected)
  }

  env <- list(a = 1)
  expect_parent( a , env, env)
  
  # function call
  env <- list(a = 1, b = 2)
  expect_parent( f(a, b) , env, env)
  
  # missing parent
  env <- list(a = 1)
  expect_parent( f(a, b) , env, env)
  
  # function
  env <- list(a = 1, f = function(...)NULL)
  expect_parent( f(a) , env, env)
  
  # recursive calls
  env <- list(a = 1, b = 2, c = 4, d = 5, e = 6, f = function(...)NULL)
  expect_parents( f(a, b, g(c, d), 3, z = e) , env, env)

  # dplyr-like verbs & lazy evaluation
  env <- list(a = 1, b = 2)
  expect_parents( iris %>% select(Species) %>% summary , env, env)
})


# TODO make replacement of literals optional?
test_that("literals are substituted", {
  expect_literals <- function (input, subst, output) {
    result <- replace_literals(substitute(input), subst)
    expected <- substitute(output)
    expect_equal(result, expected)
  }
  
  expect_literals( f(a, b) , list(a = 1), f(1, b) )
  expect_literals( f(g(h(a))) , list(a = 1), f(g(h(1))) )
  expect_literals(iris %>% filter(Sepal.Width > a), list(a = 1),
                  iris %>% filter(Sepal.Width > 1))
})


test_that("package dependencies (aka parents) are extracted", {
  skip_if_not_installed(dplyr)
  
  expect_pkg_deps(iris %>% filter(Sepal.Width > a),
                  list("iris", "filter"))
})

