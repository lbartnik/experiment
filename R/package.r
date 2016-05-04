#' State object.
#' 
#' \describe{
#'   \item{last}{Stores handles to all objects in the global namespace
#'     at the time of the last commit.}
#'   \item{tracking}{whether we are in the tracking state}
#'   \item{old_prompt}{prompt as set when loading the package}
#'   \item{stash}{local. file-system-based, object cache}
#' }
#' 
state <- new.env()


initiate_state <- function ()
{
  state$tracking       <- TRUE
  state$last_commit_id <- empty_commit()
  state$old_prompt     <- ''
  state$stash          <- storage(tempdir(), .create = TRUE)
  state$task_callback_id <- NA
  state$old_prompt     <- ''
}

TRACE_NONE <- 0


.onLoad <- function (libname, pkgname)
{
  initiate_state()
  options(experiment.trace = TRACE_NONE)
  options(experiment.set_prompt = FALSE)
  
  if (interactive() && as.logical(getOption("experiment.set_prompt"))) {
    state$task_callback_id <- addTaskCallback(update_current_commit)
    state$old_prompt <- getOption("prompt")
    update_prompt(state$tracking)
  }
}


.onUnload <- function (libpath)
{
  if (interactive()) {
    if (!is.na(state$task_callback_id)) {
      removeTaskCallback(state$task_callback_id)
      state$task_callback_id <- NA
    }
    options(prompt = state$old_prompt)
  }
}
