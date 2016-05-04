
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
update_current_commit <- function (expr, result, successful, printed)
{
  if (!state$tracking || !successful)
    return(TRUE)
  
  tryCatch({
      cmt <- create_commit(globalenv(), expr)
      lapply(names(cmt), function (name) {
        obj <- get(name, envir = globalenv(), inherits = FALSE)
        hsh <- cmt$objects[[name]]
        store_object(state$stash, hsh, obj)
      })
      state$last_commit_id <- store_object(state$stash, hash(cmt), cmt,
                                           list(parent = state$last_commit_id))
    },
    error = function(e) warning('could not create a commit: ',
                                e$message, call. = FALSE)
  )
  
  TRUE
}



# --- actual commit-related code ---------------------------------------

is_commit <- function(x) inherits(x, 'commit')

empty_commit <- function()
  structure(
    list(objects = data.frame(), history = NA, hash = '', parent = NA),
    class = 'commit'
  )


#' Generates commit from an environment.
#' 
#' @param env Environment to process.
#' @return A commit object.
#' 
create_commit <- function (env, history)
{
  lst <- eapply(env, hash)
  lst <- lst[sort(names(lst))]
  structure(list(objects = lst, history = history), class = 'commit')
}
