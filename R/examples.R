#' @export
#' @import storage
modelling <- function (overwrite = FALSE)
{
  if (length(storage::os_list(internal_state$stash))) {
    if (!isTRUE(overwrite)) {
      stop("stash is not empty and `overwrite` is FALSE, aborting",
           call. = FALSE)
    }

    warning("stash is not empty and `overwrite` is TRUE, removing data", call. = FALSE)
    storage::os_remove(internal_state$stash)
    internal_state$stash <- create_stash()
  }

  path <- file.path(system.file("examples", package = "experiment"), 'modelling')
  file.copy(from = list.files(path, full.names = TRUE),
            to = as.character(internal_state$stash),
            recursive = TRUE)

  g <- graph(internal_state$stash)
  l <- which.max(vapply(g, `[[`, numeric(1), i = 'level'))
  internal_state$last_commit <- g[[l]]$id

  invisible()
}
