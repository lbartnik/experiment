
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


#' @export
by_class <- function (cls) results(stashed(class == cls))


# --- wrappers ---


#' A single result.
result <- function (object, id)
{
  structure(list(object = object, id = id), class = 'result')
}


#' A set (list) of results.
results <- function (res)
{
  stopifnot(is.list(res), all_named(res))
  
  structure(
    mapply(result, object = res, id = names(res), SIMPLIFY = FALSE, USE.NAMES = FALSE),
    class = 'results'
  )
}



#' @export
`.DollarNames.result` <- function (x, pattern = "")
{
  grep(pattern, c('commit', 'object', 'id'), value = TRUE)
}


`$.result` <- function (x, i)
{
  if (identical(i, 'id') || identical(i, 'object')) {
    return(x[[i]])
  }
  
  if (identical(i, 'commit')) {
    g <- graph(internal_state$stash)
    g <- Filter(function (co) (x$id %in% co$object_ids), g)
    i <- which.min(vapply(g, function (co) co$level, numeric(1)))
    return(g[[i]])
  }
  
  stop('unknown option: ', i, call. = FALSE)
}


#' @export
`.DollarNames.results` <- function (x, pattern = "")
{
  ids <- vapply(x, `[[`, character(1), i = 'id')
  srt <- storage::shorten(ids)
  ids <- if (anyDuplicated(srt)) ids else srt
  
  grep(pattern, c('tidy', ids), value = TRUE)
}


#' @export
`$.results` <- function (x, i)
{
  ids <- vapply(x, `[[`, character(1), i = 'id')
  short_ids <- storage::shorten(ids)
  
  if (identical(i, 'tidy')) {
    rows <- lapply(x, function (x) broom::tidy(x$object))
    return(cbind(id = short_ids, do.call(rbind, rows)))
  }
  
  if (length(j <- match(i, short_ids))) return(x[[j]])
  if (length(j <- match(i, ids))) return(x[[j]])
  
  stop('unknown option: ', i, call. = FALSE)
}


# --- printing ---

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
