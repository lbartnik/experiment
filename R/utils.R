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

is_empty <- function (x) {
  (is.environment(x) && !length(x)) ||
    is.null(x) || is.na(x) || !length(x) || (is.character(x) && !nchar(x))
}

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


first_not_null <- function (...) {
  nn <- not_null(...)
  nth(nn, 1)
}


crc32 <- function (x) digest::digest(x, algo = 'crc32')


is_knitr <- function () getOption("knitr.in.progress", FALSE)


cat0 <- function (..., sep = '') cat(..., sep = sep)



#' Returns a base64-encoded, SVG plot.
#'
#' @param pl Plot recorded by [recordPlot()].
#' @return `character` string, base64-encoded SVG plot.
#' @import jsonlite
#'
plot_as_svg <- function (pl)
{
  if (is.null(pl)) return(NULL)

  # TODO use svglite::stringSVG

  path <- tempfile(fileext = ".svg")

  # TODO if `pl` has been recorded without dev.control("enable"), the
  #      plot might be empty; it might be necessary to check for that

  svg(path)
  replayPlot(pl)
  dev.off()

  contents <- readBin(path, "raw", n = file.size(path))
  jsonlite::base64_enc(contents)
}


# TODO could be turned into a S3 method
auto_tags <- function (obj)
{
  list(class = class(obj), time = Sys.time())
}

