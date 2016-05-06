
is_commit <- function(x) inherits(x, 'commit')


#' Convert environment into a commit.
#' 
#' @param env Environment to be stored.
#' @param parent_id ID of the parent commit.
#' @param history History line that triggered this store operation.
#' @param storage Storage object.
#' @return ID of the new commit in \code{storage}.
#' 
store_commit <- function (env, parent_id, history, storage)
{
  # TODO also store (ordered) list of currently loaded packages
  
  obj <- ls(envir = env)
  nms <- vapply(obj, function (name) {
    ob <- env[[name]]
    store_object(storage, hash(ob), ob, auto_tags(ob, env, name = name))
  }, character(1))
  names(obj) <- nms
  
  if (!is.na(parent_id)) {
    parent_commit <- restore_object(storage, parent_id)
    if (hash(parent_commit$objects) == hash(obj))
      return(parent_id)
  }
  
  commit <- structure(list(objects = obj, history = history), class = 'commit')
  store_object(storage, hash(commit), commit, auto_tags(commit, .parent = parent_id))
}
