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
  state$tracking          <- TRUE
  state$stash             <- storage(file.path(tempdir(), 'experiment-stash'), .create = TRUE)
  state$last_commit_id    <- store_commit(emptyenv(), NA_character_, bquote(), state$stash)
  state$task_callback_id  <- NA
  state$old_prompt        <- NA_character_
}

TRACE_NONE <- 0


.onLoad <- function (libname, pkgname)
{
  initiate_state()
  
  op <- options()
  op.experiment <- list(experiment.trace = TRACE_NONE,
                        experiment.set_prompt = FALSE)
  toset <- !(names(op.experiment) %in% names(op))
  if (any(toset))
    options(op.experiment[toset])
  
  state$task_callback_id <- addTaskCallback(task_callback)
  
  if (interactive() && as.logical(getOption("experiment.set_prompt"))) {
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
    if (!is.na(state$old_prompt))
      options(prompt = state$old_prompt)
  }
}
