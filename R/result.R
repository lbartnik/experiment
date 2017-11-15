

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


is_results <- function (x) inherits(x, "results")


#' @export
`.DollarNames.result` <- function (x, pattern = "")
{
  grep(pattern, c('commit', 'object', 'id'), value = TRUE)
}


#' @export
`$.result` <- function (x, i)
{
  if (identical(i, 'id') || identical(i, 'object')) {
    return(x[[i]])
  }
  
  if (identical(i, 'commit')) {
    g <- graph(internal_state$stash)
    return(find_first_parent(g, x$id))
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


# --- plotting ---

#' @export
`plot.results` <- function (res, x = 'adj.r.squared', y = 'AIC', ...)
{
  stopifnot(is_results(res))
  
  glance <- lapply(res, function (r) broom::glance(r$object))
  shared <- Reduce(intersect, lapply(glance, names))
  
  present <- function (v)
    if (!isTRUE(v %in% shared)) stop(v, ' is not available for all objects', call. = FALSE)
  
  present(x)
  present(y)
  
  X <- vapply(glance, `[[`, numeric(1), i = x)
  Y <- vapply(glance, `[[`, numeric(1), i = y)
  
  plot(X, Y, xlab = x, ylab = y)
}

