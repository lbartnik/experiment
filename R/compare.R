#' Compare models.
#' @export
compare_models <- function (x, ...) UseMethod("compare_models")


#' @export
#' @rdname compare_models
compare_models.default <- function (x, ...)
{
  models <- c(list(x), list(...))
  # TODO make sure all objects are actual models
  #      and then call the .list method
}


#' @export
#' @rdname compare_models
compare_models.clist <- function (x, ...)
{
  # TODO extract into a regular list a then call the .list method
}


#' @export
#' @rdname compare_models
compare_models.list <- function (x, ...)
{
  # TODO print all models in a scatter-plot
}


