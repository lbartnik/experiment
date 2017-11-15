
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
query_by_name <- function (value) {
  cmts <- stashed(class == 'commit')
  ids <- lapply(cmts, function (co) {
    m <- match(value, names(co$objects))
    if (!is.na(m)) return(co$objects[[m]])
    NULL
  })
  ids <- unique(unlist(ids))
  results(stashed(ids = ids))
}
