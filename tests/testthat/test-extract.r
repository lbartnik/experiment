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

  # more than one hit
  res <- search_for_assignment("x", quote({ x <- 1 ; x <- 2 ; x = 3}))
  expect_equal(res, list(quote(1), quote(2), quote(3)))
  
  # no hits
  res <- search_for_assignment("x", quote({ a <- 1 ; b <- 2 ; c = 3}))
  expect_equal(res, list())
})


test_that("user-defined parents are extracted", {
  expect_parents <- function (expr, env, expected) {
    expr    <- substitute(expr)
    parents <- extract_parents(expr, env)
    expect_equal(sort(parents), sort(expected))
  }

  env <- list(a = 1)
  expect_parents( a , env, "a")
  
  # function call
  env <- list(a = 1, b = 2)
  expect_parents( f(a, b) , env, names(env))
  
  # missing parent
  env <- list(a = 1)
  expect_parents( f(a, b) , env, "a")
  
  # function
  env <- list(a = 1, f = function(...)NULL)
  expect_parents( f(a) , env, names(env))
  
  # recursive calls
  env <- list(a = 1, b = 2, c = 4, d = 5, e = 6, f = function(...)NULL)
  expect_parents( f(a, b, g(c, d), 3, z = e) , env, names(env))

  # dplyr-like verbs & lazy evaluation
  env <- list(a = 1, b = 2)
  expect_parents( iris %>% select(Species) %>% summary , env, character())
})


# TODO make replacement of literals optional?
test_that("literals are substituted", {
  expect_literals <- function (input, subst, output) {
    input  <- substitute(input)
    result <- replace_literals(input, subst)
    expected <- substitute(output)
    expect_equal(result, expected)
  }
  
  expect_literals( f(a, b) , list(a = 1), f(1, b) )
  expect_literals( f(g(h(a))) , list(a = 1), f(g(h(1))) )
  expect_literals(iris %>% filter(Sepal.Width > a), list(a = 1),
                  iris %>% filter(Sepal.Width > 1))
})


test_that("package dependencies (aka imports) are extracted", {
  skip_if_not_installed("dplyr")
  
  expect_pkg_deps <- function (expr, deps) {
    expr <- substitute(expr)
    imports <- extract_imports(expr, globalenv())
    expect_equivalent(imports, deps)
  }
  
  expect_pkg_deps(iris %>% filter(Sepal.Width > a),
                  data.frame(package = "dplyr", func = "filter"))
})

