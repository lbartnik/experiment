temp_stash <- function (where = tempdir())
{
  name <- paste0(sample(letters, 8), collapse = "")
  path <- file.path(where, name)
  unlink(path, recursive = TRUE, force = TRUE)

  storage::filesystem(path, create = TRUE)
}


temp_filled <- function (where = tempdir())
{
  st <- temp_stash(where)
  lapply(1:10, function (x) {
    storage::os_write(st, x, list())
  })
  st
}


remove_stash <- function (path)
{
  if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
}


empty_state <- function (where = tempdir())
{
  as.environment(list(
    stash       = temp_stash(where),
    last_commit = commit(list(), bquote())
  ))
}
