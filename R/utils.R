last <- function (x) x[[length(x)]]

first <- function(x) x[[1]]

nth <- function(x, n) x[[n]]

all_named <- function (x)
{
  all(names(x) != "")
}



is_single_character <- function (x) {
  is.character(x) && (length(x) == 1)
}

is_nonempty_character <- function (x) {
  is_single_character(x) && (nchar(x) > 0)
}

is_lazy_dots <- function (x) inherits(x, 'lazy_dots')

is_empty <- function (x) is.null(x) || is.na(x) || !length(x) || (is.character(x) && !nchar(x))

is_error <- function (x) inherits(x, 'try-error')

isFALSE <- function (x) !isTRUE(x)


not_null <- function (...)
{
  x <- list(...)
  n <- lapply(x, is.null)
  p <- match(FALSE, n)
  stopifnot(!is.na(p))
  x[[p]]
}
