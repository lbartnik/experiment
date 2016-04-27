
task_callback_id <- NA

.Load <- function () {
  task_callback_id <<- addTaskCallback(update_current_commit)
}

.Unload <- function () {
  if (!is.na(task_callback_id))
    removeTaskCallback(task_callback_id)
}

