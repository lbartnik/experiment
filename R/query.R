
# --- querying ---

#' @export
stashed <- function (...)
{
  dots <- lazyeval::lazy_dots(...)
  ids <- storage::os_find(internal_state$stash, dots)
  
  objs <- lapply(ids, storage::os_read_object, store = internal_state$stash)
  names(objs) <- ids
  
  objs
}


wrap_result <- function (res)
{
  stopifnot(is.list(res), all_named(res))

  res <-
    mapply(function (obj, id) structure(list(object = obj, id = id), class = 'result'),
           obj = res, id = names(res), SIMPLIFY = FALSE, USE.NAMES = FALSE)

  class(res) <- 'results'
  res
}


#' @export
by_class <- function (cls) wrap_result(stashed(class == cls))


by_fun <- function (fun)
{
  ids <- storage::os_list(internal_state$stash)
  
  ids <- Filter(function (id) {
    obj  <- NULL
    tags <- storage::os_read_tags(internal_state$stash, id)
    isTRUE(fun(obj, tags))
  }, ids)
  
  lapply(ids, storage::os_read_object, store = internal_state$stash)
}


# --- a collection of results ---

#' @export
print.results <- function (x, ...)
{
  stopifnot(is.list(x))

  lapply(x, function (result) {
    cat(crayon::green(storage::shorten(result[['id']])), ': ')
    print(result, indent = 10)
  })
}


#' @export
`.DollarNames.list_result` <- function (x, pattern = "")
{
  ids <- vapply(x, `[[`, character(1), i = 'id')
  srt <- storage::shorten(ids)
  ids <- if (anyDuplicated(srt)) ids else srt

  grep(pattern, c('tidy', ids), value = TRUE)
}


#' @export
`$.results` <- function (x, i)
{
  if (identical(i, 'tidy')) {
    rows <- lapply(x, function (x) broom::tidy(x$object))
    ids <- vapply(x, `[[`, character(1), i = 'id')
    return(cbind(
      id = storage::shorten(ids),
      do.call(rbind, rows)
    ))
  }
  
  stop('unknown option: ', i, call. = FALSE)
}


# --- single result object ---

`.DollarNames.result` <- function (x, pattern)
{
  c('commit', 'object')
}


`$.result` <- function (x, i)
{
  if (identical(i, 'commit')) {
    by_fun(function(obj, tags) { stop('I do not have the id')})
  }

#  stop('unknown option: ', i, call. = FALSE)
}


#' @export
print.result <- function (x, ...) print_result(x[['object']], ...)


#' @export
print_result <- function (x, ...) UseMethod("print_result")

#' @export
print_result.default <- function (x, ...)
{
  print(x)
}

#' @export
print_result.lm <- function (x, digits = 2, indent = 0, ...)
{
  glance <- broom::glance(x)
  values <- format(glance, digits = digits)
  glance <- paste(names(glance), values, sep = ':', collapse = ' ')
  
  cls <- paste(class(x), collapse = ' ')
  frm <- format(x$call$formula)
  indent <- indent + nchar(cls) + nchar(frm) + 5
  
  glance <- strwrap(glance, width = getOption("width") - indent)
  if (length(glance) > 1) {
    glance <- paste0(stringi::stri_sub(glance[1], 1, -3), '...')
  }
  
  cat(crayon::red(cls),
      '',
      crayon::yellow(frm),
      ' ',
      glance,
      '\n')
}
