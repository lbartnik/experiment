
#' Combine ellipsis with optional list of dots.
#' 
#' Some functions allow for passing both ellipsis argument which they
#' convert to a \code{lazy_dots} object, and a \code{list} that needs
#' to be combined with ellipsis. This happens when \emph{lazy dots}
#' need to be passed between user-facing functions.
#' 
#' @param dots Dots created with \link[lazyeval]{\code{lazy_dots}}.
#' @param dots2 Another (optional) \emph{lazy dots} or a plain list;
#'        passed to \link[lazyeval]{\code{as.lazy_dots}}.
#' 
#' @return A combined \code{list} of lazy objects.
#' 
#' @importFrom lazyeval as.lazy_dots
#' 
combine_dots <- function (dots, dots2)
{
  if (!missing(dots2)) {
    dots2 <- as.lazy_dots(dots2)
    dots <- c(dots, dots2)
  }
  dots
}


is_error <- function (x) inherits(x, 'try-error')

