
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


#' @export
by_class <- function (cls)
{
  objs <- stashed(class == cls)
  objs <- lapply(objs, function (obj) {
    class(obj) <- c('result', class(obj))
    obj
  })
  class(objs) <- 'list_result'
  objs
}


# --- a collection of results ---

#' @export
print.list_result <- function (x, ...)
{
  stopifnot(is.list(x))
  mapply(function (obj, id) {
    cat(crayon::green(storage::shorten(id)), ': ')
    print(obj, indent = 10)
  }, obj = x, id = names(x))
}


#' @export
`.DollarNames.list_result` <- function (x, pattern = "")
{
  ids <- names(x)
  srt <- storage::shorten(ids)
  ids <- if (anyDuplicated(srt)) ids else srt

  grep(pattern, c('tidy', ids), value = TRUE)
}


#' @export
`$.list_result` <- function (x, i)
{
  if (identical(i, 'tidy')) {
    rows <- lapply(x, function (obj) { broom::tidy(`class<-`(obj, setdiff(class(obj), 'result')))})
    names(rows) <- NULL
    return(cbind(
      id = storage::shorten(names(x)),
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
print.result <- function (x, ...) print_result(x, ...)


#' @export
print_result <- function (x, ...) UseMethod("print_result")

#' @export
print_result.default <- function (x, ...)
{
  class(x) <- setdiff(x, 'result')
  print(x)
}

#' @export
print_result.lm <- function (x, digits = 2, indent = 0, ...)
{
  class(x) <- setdiff(class(x), 'result')

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
