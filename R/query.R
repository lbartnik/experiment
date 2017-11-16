
# --- querying ---

#' @export
stashed <- function (..., ids)
{
  if (missing(ids)) {
    dots <- lazyeval::lazy_dots(...)
    ids <- storage::os_find(internal_state$stash, dots)
  }

  objs <- lapply(ids, storage::os_read_object, store = internal_state$stash)
  names(objs) <- ids

  objs
}



#' @export
query_by <- function (...)
{
  dots <- lazyeval::lazy_dots(...)

  check <- list(class = function(...) "class",
                name = function(...) "name")

  what <- vapply(dots, function (le) tryCatch(lazyeval::lazy_eval(le, data = check),
                                              error = function(e) NA_character_),
                 character(1))
}


#' @export
query_by_class <- function (value) results(stashed(class == value))


#' @export
query_by_name <- function (value)
{
  cmts <- stashed(class == 'commit')
  ids <- lapply(cmts, function (co) {
    m <- match(value, names(co$objects))
    if (!is.na(m)) return(co$objects[[m]])
    NULL
  })
  ids <- unique(unlist(ids))
  results(stashed(ids = ids))
}


#' @export
#' @import storage
#' @import crayon
explain <- function (id)
{
  stopifnot(is_nonempty_character(id))
  id <- to_long(id, internal_state$stash)

  g <- graph(internal_state$stash)
  c <- find_first_parent(g, id)

  explain_parents(g, id)

  t <- storage::os_read_tags(internal_state$stash, id)
  cat("in commit", crayon::yellow(storage::shorten(c$id)), ": ", crayon::green(format(c$expr)))
}


#' @import defer
explain_parents <- function (graph, id)
{
  stopifnot(is_graph(graph))
  c <- find_first_parent(graph, id)
  if (is.na(c$parent)) return()

  p <- commit_restore(c$parent, internal_state$stash, .data = FALSE)

  f <- function(){}; body(f) <- c$expr

  d <- defer_(f, .caller_env = as.environment(p$object_ids), .extract = T)
  v <- extract_variables(d)

  if (length(v)) {
    lapply(as.character(v), function (id) explain_parents(graph, id))
  }

  cat(format(c$expr), '\n')

  invisible()
}
