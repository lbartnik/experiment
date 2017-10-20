
#' Creates a new commit object.
#'
#'
commit <- function (contents, expression, parent)
{
  structure(list(), class = 'commit')
}


is_commit <- function (x) inherits(x, 'commit')


#' Write commit to an object store.
#'
#'
commit_store <- function (commit, store)
{
  stopifnot(is_commit(commit))
  stopifnot(is_store(store))

  
}


