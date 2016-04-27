
task_callback_id <- NA

.onLoad <- function (libname, pkgname) {
  task_callback_id <<- addTaskCallback(update_current_commit)
}

.onUnload <- function () {
  if (!is.na(task_callback_id))
    removeTaskCallback(task_callback_id)
}

