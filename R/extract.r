
#' Extract RHS of assignment to a variable.
#' 
#' @param name Variable name to search for.
#' @param expression Any expression where \code{name} is expected to be
#'        assigned.
#' @return RHS of the assignment expression.
#'
extract_assignment <- function (name, expression)
{
  stopifnot(is.character(name))
  stopifnot(is.language(expression))
  
  res <- search_for_assignment(name, expression)
  if (length(res) == 1)
    return(res[[1]])
  
  NULL
}


#' Returns \code{TRUE} if an assignment expression.
#' 
#' Identifies three types of assignments: \code{<-}, \code{<<-} and
#' \code{=}. Returns \code{TRUE} is \code{x} is a \code{call} and
#' its first element is identical with either of the three.
#' 
#' @param x An object to be tested.
#' @return \code{TRUE} is \code{x} is an assignment, \code{FALSE} otherwise.
#' 
is_assignment <- function (x)
{
  is.call(x) &&
    (identical(x[[1]], quote(`<-`)) || identical(x[[1]], quote(`=`)) ||
     identical(x[[1]], quote(`<<-`)))
}


#' Recursively search for assignment to a given variable.
#' 
#' @param name Variable name.
#' @param expression Expression to search in.
#' @return An \code{expression} that produces the variable value or
#'         \code{NULL} if could not find it.
#'
search_for_assignment <- function(name, expression)
{
  recurse <- function(x) unlist(lapply(x, function(y) search_for_assignment(name, y)),
                                recursive = FALSE)
  
  if (is.atomic(expression) || is.name(expression)) {
    list()
  }
  else if (is.call(expression)) {
    ret <- list()
    if (is_assignment(expression) &&
        is.name(expression[[2]]) &&
        as.character(expression[[2]]) == name)
    {
      ret <- list(expression[[3]])
    }
    
    c(ret, recurse(expression))
  }
  else if (is.pairlist(expression)) {
    recurse(expression)
  }
  else {
    stop("Don't know how to handle type ", typeof(expression), 
         call. = FALSE)
  }
}



#' Identify objects referenced in an expression.
#' 
#' Finds all objects from \code{env} referenced in \code{expression}.
#' 
#' @param expression Expression to process.
#' @param env Environment to search for objects in.
#' @return 
#' 
extract_parents <- function (expression, env)
{
  find_globals <- function (expression)
  {
    recurse <- function(x) unlist(lapply(x, function(y) find_globals(y)),
                                  use.names = FALSE)
    
    if (is.atomic(expression) || is.name(expression)) {
      name <- as.character(expression)
      if (name %in% names(env)) name else character()
    }
    else if (is.call(expression)) {
      fun_name <- as.character(expression[[1]])
      if (!(fun_name %in% names(env)))
        fun_name <- character()
      return(c(fun_name, recurse(expression[-1])))
    }
    else if (is.pairlist(expression))
      return(recurse(expression))
    else {
      stop("Don't know how to handle type ", typeof(expression), 
           call. = FALSE)
    }
  }
  
  find_globals(expression)
}



#' Replace literals in an expression.
#' 
#' Replace all occurences of a given symbol with its value. A values needs
#' to be a single-element atomic vector.
#' 
#' @param expression A \code{language} object.
#' @param literals A named \code{list} of atomic values.
#' @return \code{expression} with respective symbols replaced with their values.
#' 
#' @importFrom lazyeval lazy_ interp
#' 
replace_literals <- function(expression, literals)
{
  stopifnot(is.language(expression))
  
  stopifnot(is.list(literals))
  stopifnot(all(nchar(names(literals)) > 0))
  stopifnot(all(vapply(literals, is.atomic, logical(1))))
  stopifnot(all(vapply(literals, length, numeric(1)) == 1))
  
  lazy_expr <- lazy_(expression, emptyenv())
  interp(lazy_expr, .values = literals)$expr
}





