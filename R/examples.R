#' @export
#' @import storage
modelling <- function (overwrite = FALSE, state)
{
  if (missing(state)) state <- internal_state

  if (length(storage::os_list(state$stash))) {
    if (!isTRUE(overwrite)) {
      stop("stash is not empty and `overwrite` is FALSE, aborting",
           call. = FALSE)
    }

    warning("stash is not empty and `overwrite` is TRUE, removing data", call. = FALSE)
    storage::os_remove(state$stash)
    state$stash <- create_stash()
  }

  path <- file.path(system.file("examples", package = "experiment"), 'modelling')
  file.copy(from = list.files(path, full.names = TRUE),
            to = as.character(state$stash),
            recursive = TRUE)

  g <- graph(state$stash)
  l <- which.max(vapply(g, `[[`, numeric(1), i = 'level'))
  state$last_commit <- g[[l]]$id

  invisible()
}
