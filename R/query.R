
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
