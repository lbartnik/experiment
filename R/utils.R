nth <- function(x, n) {
  if (!length(x)) return(vector(mode = typeof(x)))
  x[[n]]
}

last <- function (x) nth(x, length(x))

first <- function(x) nth(x, 1)


between <- function (x, a, b) all(a <= x) && all(x <= b)


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



is_knitr <- function () getOption("knitr.in.progress", FALSE)
