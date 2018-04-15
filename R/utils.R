`%nin%` <- function (x, table) match(x, table, nomatch = 0L) == 0L

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


# returns the first non-null element in args (...)
not_null <- function (...)
{
  x <- list(...)
  n <- lapply(x, is.null)
  p <- match(FALSE, n)
  stopifnot(!is.na(p))
  x[[p]]
}


with_class <- function (x, ...)
{
  classes <- list(...)
  stopifnot(all(vapply(classes, is.character, logical(1))))

  class(x) <- c(as.character(classes), class(x))
  x
}


crc32 <- function (x) digest::digest(x, algo = 'crc32')

pack <- function (...) paste(list(...), collapse = '::')

unpack <- function (x) stringi::stri_split_fixed(x, '::')


is_knitr <- function () getOption("knitr.in.progress", FALSE)



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


cat0 <- function (..., sep = '') cat(..., sep = sep)

ccat <- function (color, ..., sep = ' ')
{
  if (identical(color, 'default'))
    cat(..., sep = sep)
  else {
    color <- get(color, envir = asNamespace("crayon"), inherits = FALSE)
    cat(color(paste(..., sep = sep)))
  }
}

ccat0 <- function (color, ...) ccat(color, ..., sep = '')


ccat_ <- function (chunks, sep = ' ')
{
  mapply(color = names(chunks), chunk = chunks,
         function (color, chunk)
  {
    ccat0(color, paste(chunk, collapse = sep))
  })
}


log <- function (level, ...) {
  ccat0("red", '[', level, '] ', ..., '\n')
}

debug <- function (...) {
  if (isTRUE(getOption("experiment.debug"))) log("DEBUG", ...)
}

guard <- function () {
  x <- sys.call(-1)[[1]]
  fname <- if (is.symbol(x)) deparse(x) else '<unnamed>'
  debug("-> ", fname, '()')

  parent <- sys.frame(sys.parent(1))
  expr <- substitute(debug(x), list(x = paste0('<- ', fname, '()')))
  do.call(on.exit, list(expr = expr, add = TRUE), envir = parent)

  invisible()
}




splice <- function (x, ...)
{
  args <- list(...)
  stopifnot(names(args) == 'after')

  i <- which(names(x) == args$after)
  stopifnot(length(i) == 1)

  x[seq(i+1, length(x))]
}


napply <- function (lst, f, ...) {
  stopifnot(is.list(lst), all_named(lst))
  ans <- mapply(name = names(lst), value = lst, function (name, value) f(name, value, ...),
                SIMPLIFY = FALSE, USE.NAMES = FALSE)
  names(ans) <- names(lst)
  ans
}



