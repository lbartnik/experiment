#' @importFrom utils .DollarNames
NULL


.onLoad <- function (libname, pkgname)
{
  initiate_state()
}

.onUnload <- function (libpath)
{
  tracking_off()
}
