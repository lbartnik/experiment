#' @export
#' @import storage
modelling <- function ()
{
  path <- file.path(system.file("examples", package = "experiment"), 'modelling')
  storage::filesystem(path, create = FALSE)
}
