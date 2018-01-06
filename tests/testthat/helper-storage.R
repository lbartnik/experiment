empty_state <- function (where = tempdir())
{
  as.environment(list(
    stash       = empty_store(where),
    last_commit = commit(list(), bquote())
  ))
}

remove_store <- function (path)
{
  if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
}


empty_store <- function (where = tempdir())
{
  name <- paste0(sample(letters, 8), collapse = "")
  path <- file.path(where, name)
  unlink(path, recursive = TRUE, force = TRUE)

  storage::filesystem(path, create = TRUE)
}


filled_store <- function (where = tempdir())
{
  st <- empty_store(where)
  lapply(1:10, function (x) {
    storage::os_write(st, x, list())
  })
  st
}


fill_commits <- function (store)
{
  c <- commit(list(x = 1), bquote(), NA_character_, 'a', list(x = 'p'))
  commit_store(c, store)

  c <- commit(list(x = 1, y = 2L), bquote(), 'a', 'b', list(x = 'p', y = 'q'))
  commit_store(c, store)

  c <- commit(list(x = '3', y = 2L), bquote(), 'b', 'c', list(x = 'r', y = 'q'))
  commit_store(c, store)

  c <- commit(list(x = '3', y = 2L, z = 1), bquote(), 'c', 'd', list(x = 'r', y = 'q', z = 's'))
  commit_store(c, store)

  store
}


commit_memory_store <- function () fill_commits(storage::memory())

commit_filesystem_store <- function (where = tempdir()) fill_commits(empty_store(where))



sample_graph <- function ()
{
  structure(list(a = list(id         = 'a',
                          parent     = 'b',
                          objects    = list(x = '3', y = 2L),
                          object_ids = list(x = 'r', y = 'q')),
                 b = list(id         = 'b',
                          parent     = 'c',
                          objects    = list(x = 1, y = 2L),
                          object_ids = list(x = 'p', y = 'q')),
                 c = list(id         = 'c',
                          parent     = NA,
                          objects    = list(x = 1),
                          object_ids = list(x = 'p'))),
            class = 'graph')
}


empty_steps <- function ()
{
  structure(list(steps = list(), links = list()), class = 'steps')
}


add_step <- function (steps, id, parent, type = 'object', ...)
{
  step <- c(list(id = id, parent = parent, type = type), list(...))
  stopifnot(all(c('id', 'parent') %in% names(step)))

  steps$steps <- c(steps$steps, list(step))
  if (!is.na(parent)) {
    steps$links <- c(steps$links, list(list(source = step$parent, target = step$id)))
  }

  steps
}



expect_exists <- function (what, where, info = NULL)
{
  what_label  <- quasi_label(rlang::enquo(what))
  where_label <- quasi_label(rlang::enquo(where))

  cmp <- vapply(where, identical, logical(1), y = what)
  expect(any(cmp), sprintf("%s does not exist in %s", what_label$lab, where_label$lab),
         info = info)
}

