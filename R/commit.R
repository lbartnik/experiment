
#' Creates a new commit object.
#'
#'
commit <- function (contents, expression, parent, id = NA_character_)
{
  objects <- as.list(contents)
  stopifnot(all_named(objects))

  structure(list(id = id, objects = objects, expr = expression, parent = parent),
            class = 'commit')
}


is_commit <- function (x) inherits(x, 'commit')


#' @export
#' @rdname commit
print.commit <- function (x, simple, ...)
{
  if (isTRUE(simple))
  {
    cat(substr(x$id, 1, 8), ': ', names(x$objects), '\n')
  }
  else
  {
    cat('Commit    : ', ifelse(is.na(x$id), 'NA', x$id), '\n')
    cat('  objects : ', names(x$objects), '\n')
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
    id <- storage::compute_id(o)
    if (storage::os_exists(store, id)) return(id)

    storage::os_write(store, o, id = id)
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



commit_restore <- function (id, store)
{
  stopifnot(is_nonempty_character(id),
            storage::is_object_store(store))
  
  co <- storage::os_read(store, id)
  objects <- lapply(co$object$objects, storage::os_read_object,
                    store = store)
  
  commit(objects, co$object$expr, co$tags$parent, id)
}
