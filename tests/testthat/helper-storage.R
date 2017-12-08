temp_stash <- function ()
{
  name <- paste0(sample(letters, 8), collapse = "")
  path <- file.path(tempdir(), name)
  unlink(path, recursive = TRUE, force = TRUE)

  storage::filesystem(path, create = TRUE)
}

empty_state <- function ()
{
  as.environment(list(
    stash       = temp_stash(),
    last_commit = commit(list(), bquote()),
    plots       = list()
  ))
}
