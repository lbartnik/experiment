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
    char_tags <- vapply(tags, as.character, character(1))
    cat(paste(names(tags), char_tags, sep = ":", collapse = " "), '\n')
  })
  invisible()
}


#' Print the history of commits in stash.
#' 
#' @export
commits <- function () restore_all_commits(state$stash)


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
  cmts  <- restore_all_commits(state$stash)
  short <- shorten(names(cmts))
  stopifnot(id %in% short)
  
  idx <- match(id, short)
  cmt <- cmts[[idx]]
  state$last_commit_id <- cmt$id
  
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
#' @importFrom ggnetwork ggnetwork geom_edges geom_nodes geom_nodelabel_repel theme_blank
#' @importFrom ggplot2 aes arrow ggplot xlim
#' 
commit_graph <- function ()
{
  cmts <- restore_all_commits(state$stash)
  cmts <- cmts[order(vapply(cmts, `[[`, numeric(1), 'time'))]
  prnt <- vapply(cmts, `[[`, character(1), 'parent')

  parents  <- match(prnt, names(cmts))
  
  m <- matrix(0, length(parents), length(parents))
  s <- matrix(c(seq_along(parents), parents), ncol = 2)
  m[s] <- 1
  n <- as.network(m, directed = TRUE)

  # recursively assign branch number
  mark_branch <- function (id, br) {
    if (!is.na(id) && is.null(cmts[[id]]$branch)) {
      cmts[[id]]$branch <<- br
      Recall(cmts[[id]]$parent, br)
    }
  }
  
  # for each leaf assign a branch number
  leaves <- sort(setdiff(seq_along(parents), parents))
  lapply(leaves, function(id) mark_branch(names(cmts)[[id]], id - min(leaves)))
  
  # assign coordinates: time-wise rank and branch number
  coords <- matrix(c(seq_along(cmts) * 3, vapply(cmts, `[[`, numeric(1), 'branch')),
                   ncol = 2)

  # set attributes used to pretty-print
  capture_print <- function(x)capture.output(print(x))
  set.vertex.attribute(n, 'vertex.name', vapply(cmts, capture_print, character(1)))
  set.vertex.attribute(n, 'branch', as.character(vapply(cmts, `[[`, numeric(1), 'branch')))
    
  n <- ggnetwork(n, layout = coords)
  
  ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(color = "grey", arrow = arrow(length = unit(10, "pt"), type = "closed")) +
    geom_nodes(aes(color = branch)) +
    geom_nodelabel_repel(aes(label = vertex.name), point.padding = unit(1, 'lines')) +
    xlim(-.5, 1.5) +
    theme_blank()
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
