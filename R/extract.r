
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



is_assignment <- function (x)
{
  identical(x[[1]], quote(`<-`)) || identical(x[[1]], quote(`=`)) ||
    identical(x[[1]], quote(`<<-`))
}



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
