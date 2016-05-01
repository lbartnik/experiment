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
  state$tracking   <- TRUE
  state$last       <- empty_commit()
  state$old_prompt <- ''
  state$stash      <- collection(tempdir(), .create = TRUE)
  state$task_callback_id <- NA
}

TRACE_NONE <- 0


.onLoad <- function (libname, pkgname)
{
  initiate_state()
  options(experiment.trace = TRACE_NONE)
  
  if (interactive()) {
    state$task_callback_id <- addTaskCallback(update_current_commit)
    options(old_prompt = getOption("prompt"))
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
    options(prompt = getOption("old_prompt"), old_prompt = NULL)
  }
}
