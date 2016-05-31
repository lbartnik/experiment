# --- interactive part of keeping track of commits ---------------------

#' Set tracking mode to ON/OFF.
#' 
#' @export
#' 
tracking_on <- function () {
  state$tracking <- TRUE
  update_prompt(state$tracking)
}


#' @name tracking_on
#' @export
tracking_off <- function () {
  state$tracking <- FALSE
  update_prompt(state$tracking)
}


#' Updates the prompt according to \code{state$tracking}.
#' 
update_prompt <- function (on_off) {
  if (on_off)
    options(prompt = "tracking > ")
  else
    options(prompt = "not tracking > ")
}


#' Print stash.
#' 
#' @export
stashed <- function ()
{
  lapply(restore_tags_by(state$stash, class != 'commit'), function (tags) {
    cat(paste(names(tags), as.character(tags), sep = ":", collapse = " "), '\n')
  })
  invisible()
}


#' Print the history of commits in stash.
#' 
#' @export
commits <- function (st = state$stash)
{
  cmts <- restore_objects_by(st, class == 'commit')
  tags <- lapply(names(cmts), function(id)restore_tags(st, id))
  idx  <- order(vapply(tags, `[[`, numeric(1), 'time'))
  structure(cmts[idx], class = 'commit_set')
}


#' @importFrom lazyeval lazy_dots
#' @export
stash_restore <- function (...) {
  dots <- lazy_dots(...)
  restore_objects_by(state$stash, dots = dots)
}


#' Restore state of work from before into global environment.
#' 
#' @param id Commit identifier.
#' @param clear Clear global environment before restoring the commit.
#' 
#' @export
checkout_commit <- function (id, clear = TRUE)
{
  cmts  <- commits()
  short <- shorten(names(cmts))
  stopifnot(id %in% short)
  
  idx <- match(id, short)
  cmt <- cmts[[idx]]
  tgs <- restore_tags(state$stash, names(cmts)[[idx]])
  state$last_commit_id <- ifelse('parent' %in% names(tgs), tgs$parent, NA_character_)
  
  obj_ids <- names(cmt$objects)
  objects <- lapply(obj_ids, function(id)restore_object(state$stash, id))
  names(objects) <- cmt$objects

  if (clear)
    rm(list = ls(globalenv()), envir = globalenv())
  list2env(objects, envir = globalenv())
  
  cmt
}


#' @export
#' @importFrom network as.network set.vertex.attribute
#' @importFrom ggnetwork ggnetwork geom_edges geom_nodes geom_nodelabel
#' @importFrom ggplot2 ggplot aes
#' 
commit_graph <- function ()
{
  cmts <- commits()
  tags <- lapply(names(cmts), function(id)restore_tags(state$stash, id))
  prnt <- vapply(tags, `[[`, character(1), 'parent')

  parents  <- match(prnt, names(cmts))
  
  m <- matrix(0, length(parents), length(parents))
  s <- matrix(c(seq_along(parents), parents), ncol = 2)
  m[s] <- 1
  n <- as.network(m, directed = TRUE)
  set.vertex.attribute(n, 'vertex.name', shorten(names(cmts)))
  n <- ggnetwork(n)

  ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(color = "grey", arrow = arrow(length = unit(10, "pt"), type = "closed")) +
    geom_nodes(color = 'white') +
    geom_nodelabel(aes(label = vertex.name))
}



# Creating a new commit/checkout:
#
#  1. get the last commit
#  2. get the list of object names and see if any of them overlap with
#     the last commit
#  3. those that overlap we already have hashed in the last commit
#  4. those that have the same names but new contents need to be hashed
#  5. all new objects need to be hashed
#  6. put in the new commit:
#       - all names and their hashes
#       - the hash (id) of the last commit
#       - the contents of the history line (command) associated with creating
#         this new commit
#     (do we create a commit if none of the objects were changed? if the
#      history line is just printing something or opening help? rather not)
#  7. put the commit in the storage space
#  8. store all new objects and the commit object in the storage space


#' @export
update_current_commit <- function (env, history)
{
  state$last_commit_id <-
    store_commit(env, state$last_commit_id, history, state$stash)
}


#' A callback run after each top-level expression is evaluated.
#'  
#' From \link{\code{addTaskCallback}}: if the data argument was
#' specified in the call to addTaskCallback, that value is given as the
#' fifth argument.
#'
#' @param expr Expression for the top-level task.
#' @param result Result of the top-level task.
#' @param successful A logical value indicating whether it was
#'        successfully completed or not (always \code{TRUE} at present).
#' @param printed A logical value indicating whether the result was
#'        printed or not.
#'
#' @return A logical value indicating whether to keep this function in
#'         the list of active callback.
#'         
task_callback <- function (expr, result, successful, printed)
{
  if (!state$tracking || !successful)
    return(TRUE)
  
  tryCatch(
    error = function(e) warning('could not create a commit: ',
                                e$message, call. = FALSE),
    {
      # it's length of ls() because we don't care for hidden objects
      if (length(ls(globalenv())))
        update_current_commit(globalenv(), expr)
    }
  )
  
  TRUE
}
