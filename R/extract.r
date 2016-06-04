
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








