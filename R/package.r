
task_callback_id <- NA


.onLoad <- function (libname, pkgname)
{
  initiate_state()
  
  if (interactive()) {
    task_callback_id <<- addTaskCallback(update_current_commit)
    options(old_prompt = getOption("prompt"))
    update_prompt(state$tracking)
  }
}


.onUnload <- function (libpath)
{
  if (interactive()) {
    if (!is.na(task_callback_id))
      removeTaskCallback(task_callback_id)
    options(prompt = getOption("old_prompt"), old_prompt = NULL)
  }
}
