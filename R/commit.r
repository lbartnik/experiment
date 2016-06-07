
#' Create a commit object.
#' 
#' @param env Environment to create this commit from.
#' @param history Expression where a new object might have been created.
#' @param storage Storage where newly created objects (if any) will be stored.
#' 
#' @return A \code{commit} object.
#' 
create_commit <- function (env, history, storage)
{
  prepare_object <- function (name)
  {
    ob <- env[[name]]
    id <- hash(ob)
    if (object_exists(storage, id))
      return(id)
    
    expr    <- extract_assignment(name, history)

    if (is.atomic(expr)) {
      parents <- list()
    }
    else if (is.language(expr)) {
      parents <- extract_parents(expr, env)
      expr    <- replace_literals(expr, extract_literals(env))
    }
    else {
      parents <- list()
      expr    <- NULL
      warning("could not extract assignment and parent for object '", name, "'",
              call. = FALSE)
    }
    # TODO extrct imports, too
    
    store_object(storage, id, ob, auto_tags(ob, env,
                                            name       = name,
                                            assignment = expr,
                                            parents    = parents))
  }
  
  # generate list of objects; hash sums are used as names
  object_names <- ls(envir = env, sorted = TRUE)
  object_ids   <- vapply(object_names, prepare_object, character(1))
  names(object_names) <- object_ids
  
  structure(list(objects = object_names), class = 'commit')
}


#' Does an object inherit from \code{commit}.
#' 
#' @param x Object to be tested.
#' @return \code{TRUE} if \code{x} is a commit.
#' 
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
  
  commit <- create_commit(env, history, storage)
  
  # if parent holds the same list of obejcts, don't store
  if (!is.na(parent_id)) {
    parent_commit <- restore_object(storage, parent_id)
    if (hash(parent_commit$objects) == hash(commit$objects))
      return(parent_id)
  }

  # store list of objects and line of history, the rest goes as tags
  # TODO rename "parent" to "parents" when parents for regular object are
  #      implemented
  store_object(storage, hash(commit), commit,
               auto_tags(commit,
                         parent  = as.character(parent_id),
                         history = history))
}


#' Read a commit from storage.
#' 
#' Commit read from storage has 
#' 
#' @param st Storage to read from.
#' @param id Commit identifier.
#' 
#' @return A \code{commit} object.
#' 
restore_commit <- function (st, id)
{
  stopifnot(object_exists(st, id))
  
  commit <- c(restore_object(st, id), restore_tags(st, id))
  class(commit) <- commit$class
  commit$class  <- NULL
  commit$id     <- id
  
  commit
}


#' Print a \code{commit} object.
#' 
#' @export
print.commit <- function (x)
{
  stopifnot(is_commit(x))
  cat(shorten(x$id), " :  ", paste(x$objects, collapse = ', '), '\n', sep = "")
  invisible(x)
}


is_commit_set <- function (x) inherits(x, 'commit_set')


#' Restore all commits.
#' 
#' @rdname restore_commit
#' @return A \code{commit_set} object.
restore_all_commits <- function (st)
{
  ids  <- names(restore_tags_by(st, class == 'commit'))
  cmts <- lapply(ids, function(id)restore_commit(st, id))
  names(cmts) <- ids
  structure(cmts, class = 'commit_set')
}



#' @export
print.commit_set <- function (x)
{
  # TODO print in the order of creation - resolve order by looking at parent
  stopifnot(is_commit_set(x))
  
  idx  <- order(vapply(x, `[[`, numeric(1), 'time'))
  
  mapply(function (no, cm) {
    cat(no, ".", sep = '')
    print(cm)
  }, no = seq_along(x[idx]), cm = x[idx])
  
  invisible(x)
}


