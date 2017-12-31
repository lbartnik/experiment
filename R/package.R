#' @importFrom utils .DollarNames
NULL

#' @useDynLib subprocess, .registration = TRUE
NULL

.onLoad <- function (libname, pkgname)
{
  initiate_state()
}

.onUnload <- function (libpath)
{
  tracking_off()
}
