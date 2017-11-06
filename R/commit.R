
#' Creates a new commit object.
#'
#'
commit <- function (contents, expression, parent, id, object_ids)
{
  objects <- as.list(contents)
  stopifnot(all_named(objects))
  
  if (missing(id)) id <- NA_character_
  if (missing(object_ids)) object_ids <- lapply(contents, function (x) NA_character_)

  structure(list(id = id, objects = objects, object_ids = object_ids,
                 expr = expression, parent = parent),
            class = 'commit')
}


is_commit <- function (x) inherits(x, 'commit')


#' @export
#' @rdname commit
print.commit <- function (x, simple = FALSE, ...)
{
  if (isTRUE(simple))
  {
    cat(substr(x$id, 1, 8), ': ', names(x$objects), '\n')
  }
  else
  {
    tag_print <- function (x) {
      if (!length(x)) return('')
      if (length(x) < 2) return(as.character(x))
      paste0('[', paste(x, collapse = ', '), ']')
    }
    
    cat('Commit : ', ifelse(is.na(x$id), '<no id>', x$id), '\n')
    cat('objects :\n')
    mapply(function (name, id) {
      tags <- storage::os_read_tags(internal_state$stash, id)
      tags <- vapply(tags, tag_print, character(1))
      cat('  ', name, ': ', paste(names(tags), '=', tags, collapse = ', '), '\n')
    }, name = names(x$objects), id = as.character(x$object_ids))
    cat('\n')
  }
}



commit_equal <- function (a, b)
{
  stopifnot(is_commit(a), is_commit(b))
  setequal(a$objects, b$objects)
}



#' Write commit to an object store.
#'
commit_store <- function (commit, store)
{
  stopifnot(is_commit(commit))
  stopifnot(storage::is_object_store(store))

  # name -> ID in object store
  objects <- lapply(commit$objects, function (o) {
    o  <- cleanup_object(o)
    id <- storage::compute_id(o)
    if (storage::os_exists(store, id)) return(id)

    tg <- auto_tags(o)
    storage::os_write(store, o, id = id, tags = tg)
  })

  if (is.na(commit$id))
  {
    commit$id <- storage::compute_id(commit)
  }
  
  # this should never happen because hash is computed from
  # both objects and parent id; if it does happen, something
  # is seriously broken
  if (storage::os_exists(store, commit$id))
  {
    stop("commit already exists, aborting", call. = FALSE)
  }

  # store list of object pointers + basic 'history' tags  
  id <- storage::os_write(store, list(objects = objects, expr = commit$expr),
                          tags = list(class = 'commit', parent = commit$parent),
                          id = commit$id)

  commit
}


# TODO could be turned into a S3 method
auto_tags <- function (obj)
{
  list(class = class(obj), time = Sys.time())
}


#' Removes references to environments.
#' 
#' Some objects (e.g. formula, lm) store references to environments
#' in which they were created. This function replaces each such reference
#' with a reference to `emptyenv()`.
#' 
#' @param obj Object to be processed.
#' @return `obj` with environment references replaced by `emptyenv()`
#' 
cleanup_object <- function (obj)
{
  if (is.environment(obj)) return(emptyenv())
  
  attrs <- lapply(attributes(obj), cleanup_object)
  
  if (is.list(obj))
  {
    obj <- lapply(obj, cleanup_object)
  }
  
  attributes(obj) <- attrs
  obj
}



commit_restore <- function (id, store, .data = TRUE)
{
  stopifnot(is_nonempty_character(id),
            storage::is_object_store(store))
  
  co <- storage::os_read(store, id)

  if (isTRUE(.data)) {
    object_restore <- function (id) storage::os_read_object(store, id)
  }
  else {
    object_restore <- function (id) NA_character_  
  }
  
  objects <- lapply(co$object$objects, object_restore)
  commit(objects, co$object$expr, co$tags$parent, id, co$object$objects)
}
