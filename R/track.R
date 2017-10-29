# Track user's interactions with R session.

#' Global state of the tracker.
#' 
#' \describe{
#'   \item{tracking}{whether we are in the tracking state}
#'   \item{old_prompt}{prompt as set when loading the package}
#'   \item{stash}{local, file-system-based, object cache}
#' }
#' 
internal_state <- new.env()



initiate_state <- function ()
{
  internal_state$tracking         <- FALSE
  internal_state$stash            <- storage::filesystem(file.path(tempdir(), 'experiment-stash'),
                                                         create = TRUE)
  internal_state$task_callback_id <- NA
  internal_state$old_prompt       <- getOption("prompt")
  internal_state$last_commit      <- commit(list(), bquote(), NA_character_)
#  internal_state$last_commit_id   <- store_commit(emptyenv(), NA_character_, bquote(), state$stash)
}



#' A callback run after each top-level expression is evaluated.
#'  
#' From [addTaskCallback()]: if the data argument was specified in
#' the call to addTaskCallback, that value is given as the fifth
#' argument.
#'
#' @param expr Expression for the top-level task.
#' @param result Result of the top-level task.
#' @param successful A logical value indicating whether it was
#'        successfully completed or not (always `TRUE` at present).
#' @param printed A logical value indicating whether the result was
#'        printed or not.
#'
#' @return A logical value indicating whether to keep this function in
#'         the list of active callbacks.
#'         
task_callback <- function (expr, result, successful, printed)
{
  if (!isTRUE(internal_state$tracking) || !isTRUE(successful))
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


#' @description `update_current_commit` is a part of `task_callback`
#' made separate due to testing purposes.
#'
#' @param env Environment this commits represents.
#' @param expr Expression that created this environment.
#'
#' @name task_callback
#' @export
update_current_commit <- function (env, expr)
{
  co <- commit(as.list(env), expression, internal_state$last_commit$id)
  if (!commit_equal(co, internal_state$last_commit))
  {
    internal_state$last_commit <- commit_store(co, internal_state$stash)
  }
}




#' Toggle tracking mode.
#'
#' @export
#' 
tracking_on <- function () {
  internal_state$tracking <- TRUE
  options(prompt = "[tracked] > ")
}


#' @name tracking_on
#' @export
tracking_off <- function () {
  internal_state$tracking <- FALSE
  options(prompt = internal_state$old_prompt)
}
