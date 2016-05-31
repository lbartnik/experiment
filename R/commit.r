
#' Does an object inherit from \code{commit}.
is_commit <- function(x) inherits(x, 'commit')


#' Convert environment into a commit.
#' 
#' A commit in the filesystem is just another regular object with two
#' mandatory tags: \code{class == 'commit'} and \code{parent} that
#' points to the parent commit. Thus, reading a commit from storage
#' does not differ from reading any other object.
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
  
  # TODO refactor this into two functions: (1) create commit, (2) store commit
  
  # generate list of objects; names are object hash sums
  objects <- ls(envir = env, sorted = TRUE)
  object_names <- vapply(objects, function (name) {
    ob <- env[[name]]
    id <- hash(ob)
    if (object_exists(storage, id))
      return(id)
    store_object(storage, id, ob, auto_tags(ob, env, name = name))
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


#' Read a commit from storage.
#' 
#' Commit read from storage has 
#' 
#' @param st Storage to read from.
#' @param id Commit identifier.
restore_commit <- function (st, id)
{
  stopifnot(object_exists(st, id))
  
  commit <- c(restore_object(st, id), restore_tags(st, id))
  class(commit) <- commit$class
  commit$class <- NULL
  
  commit
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


