
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
  
  # generate list of objects; names are object hash sums
  objects <- ls(envir = env, sorted = TRUE)
  object_names <- vapply(objects, function (name) {
    ob <- env[[name]]
    store_object(storage, hash(ob), ob, auto_tags(ob, env, name = name))
  }, character(1))
  names(objects) <- object_names
  
  # if parent holds the same list of obejcts, don't store
  if (!is.na(parent_id)) {
    parent_commit <- restore_object(storage, parent_id)
    if (hash(parent_commit$objects) == hash(objects))
      return(parent_id)
  }
  
  # store list of objects and line of history, the rest goes as tags
  commit <- structure(list(objects = objects, history = history), class = 'commit')
  store_object(storage, hash(commit), commit,
               auto_tags(commit, parent = as.character(parent_id)))
}


#' @export
print.commit_set <- function (x)
{
  # TODO print in the order of creation - resolve order by looking at parent
  
  mapply(function (no, cm, id) {
    cat(no, ".", id, " :  ", paste(cm$objects, collapse = ', '), '\n', sep = "")
  }, no = seq_along(x), cm = x, id = shorten(names(x)))
  
  invisible(x)
}


