#' Compare models.
#'
#' @param x List of models.
#' @param ... Additinal parameters.
#'
#' @export
#'
#' @rdname compare
#'
compare_models <- function (x, ...) UseMethod("compare_models")



#' @export
#' @rdname compare
#'
compare_models.default <- function (x, ...)
{
  models <- c(list(x), list(...))
  # TODO make sure all objects are actual models
  #      and then call the .list method
}


#' @export
#' @rdname compare
#'
compare_models.clist <- function (x, ...)
{
  # TODO extract into a regular list a then call the .list method
}


#' @export
#' @rdname compare
#'
compare_models.list <- function (x, ...)
{
  # TODO print all models in a scatter-plot
}


