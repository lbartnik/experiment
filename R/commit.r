
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
  # TODO also store (ordered according to the at-the-time state)
  #      list of currently loaded packages
  
  obj <- ls(envir = env)
  nms <- vapply(obj, function (name) {
    ob <- env[[name]]
    store_object(storage, hash(ob), ob, auto_tags(ob, env, name = name))
  }, character(1))
  names(obj) <- nms
  
  # if the parent has the same list of objects, don't store anything
  if (!is.na(parent_id)) {
    parent_commit <- restore(storage, parent_id)
    if (hash(parent_commit$object$objects) == hash(obj))
      return(parent_id)
    
    # if the parent has children, create a new branch; if what we are
    # creating is the only child, use parent's branch number
    children <- restore_objects_by(storage,
                                   .parent == as.character(parent_id))
    
    branch_no <- ifelse(length(children) > 0,
                        count_branches(storage) + 1,
                        parent_commit$tags$branch)
  }
  # if there has been no parent so far, it's the first and only branch
  else {
    branch_no <- 1 
  }
  
  # prepare & store the commit object
  commit <- structure(list(objects = obj, history = history), class = 'commit')
  tags <- auto_tags(commit,
                    .parent = as.character(parent_id),
                    branch  = branch_no)
  store_object(storage, hash(commit), commit, tags)
}


#' Restore state of work from before into global environment.
#' 
#' @param id Commit identifier.
#' @param clear Clear global environment before restoring the commit.
#' 
#' @export
restore_commit <- function (id, clear = TRUE)
{
  cmts  <- read_commits(state$stash)
  short <- shorten(names(cmts))
  stopifnot(id %in% short)
  
  idx <- match(id, short)
  cmt <- cmts[[idx]]
  state$last_commit_id <- ifelse('.parent' %in% names(cmt), cmt$.parent, NA_character_)
  
  obj_ids <- names(cmt$objects)
  objects <- lapply(obj_ids, function(id)restore_object(state$stash, id))
  names(objects) <- cmt$objects
  
  if (clear)
    rm(list = ls(globalenv()), envir = globalenv())
  list2env(objects, envir = globalenv())
  
  cmt
}


#' Read the history of commits in stash.
read_commits <- function (st)
{
  cmts <- restore_by(st, class == 'commit')
  cmts <- lapply(cmts, function(x) c(x$object, x$tags))
  
  structure(cmts, class = 'commit_set')
}


#' @export
print.commit_set <- function (x)
{
  # TODO print in the order of creation - resolve order by looking at .parent
  idx  <- order(vapply(x, `[[`, numeric(1), 'time'))

  x <- x[idx]
  mapply(function (no, cm, id) {
    cat(no, ".", id, " :  ", paste(cm$objects, collapse = ', '), '\n', sep = "")
  }, no = seq_along(x), cm = x, id = shorten(names(x)))
  
  invisible(x)
}

