
#' Creates a new commit object.
#'
#'
commit <- function (contents, expression, parent)
{
  objects <- as.list(contents)
  stopifnot(all_named(objects))

  structure(list(objects = objects, expr = expression, parent = parent),
            class = 'commit')
}


is_commit <- function (x) inherits(x, 'commit')


#' Write commit to an object store.
#'
commit_store <- function (commit, store)
{
  stopifnot(is_commit(commit))
  stopifnot(storage::is_object_store(store))

  # name -> ID in object store
  objects <- lapply(commit$objects, function (o) {
    id <- storage::hash(o)
    if (os_exists(store, id)) return(id)

    os_write(store, o, id = id)
  })


  id <- os_write(store, list(objects = objects, expr = commit$expr),
                 tags = list(parent = commit$parent))
  id
}


