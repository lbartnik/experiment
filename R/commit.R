
#' Creates a new commit object.
#'
#'
commit <- function (contents, expression, parent)
{
  objects <- as.list(contents)
  stopifnot(all_named(objects))

  structure(list(id = NA_character_, objects = objects, expr = expression, parent = parent),
            class = 'commit')
}


is_commit <- function (x) inherits(x, 'commit')


#' @export
#' @rdname commit
commit.print <- function (x)
{
  cat('Commit    : ', ifelse(is.na(x$id), 'NA', x$id), '\n')
  cat('  objects : ', names(x$objects))
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


  id <- storage::os_write(store, list(objects = objects, expr = commit$expr),
                          tags = list(parent = commit$parent))
  commit$id <- id
  commit
}


