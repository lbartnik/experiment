#' @importFrom utils .DollarNames
NULL

# Suppress checks.
utils::suppressForeignCheck(c('LCLid', 'tstp', 'energy(kWh/hh)', 'meter', 'timestamp', 'usage', 'dow'))


.onLoad <- function (libname, pkgname)
{
  initiate_state()
  internal_state$task_callback_id <- addTaskCallback(task_callback)
  
  if (interactive() && FALSE) {
    tracking_on()
  }
}


.onUnload <- function (libpath)
{
  if (interactive()) {
    if (!is.na(internal_state$task_callback_id)) {
      removeTaskCallback(internal_state$task_callback_id)
      internal_state$task_callback_id <- NA
    }

    tracking_off()
    internal_state$old_prompt <- NA
  }
}


