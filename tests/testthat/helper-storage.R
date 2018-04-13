empty_state <- function (where = tempdir())
{
  as.environment(list(
    stash       = empty_store(where),
    last_commit = commit(list(), bquote(), NA_character_)
  ))
}

empty_memory_state <- function ()
{
  as.environment(list(
    stash       = storage::memory(),
    last_commit = commit(list(), bquote(), NA_character_)
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
  storage::os_write(store,  1,  auto_tags(1),   'p')
  storage::os_write(store,  2L, auto_tags(2L),  'q')
  storage::os_write(store, '3', auto_tags('3'), 'r')
  storage::os_write(store,  1,  auto_tags(1),   's')

  c <- commit(list(x = 'p'), bquote(x <- 1), NA_character_, 'a')
  write_commit(store, c)

  c <- commit(list(x = 'p', y = 'q'), bquote(y <- 2L), 'a', 'b')
  write_commit(store, c)

  c <- commit(list(x = 'r', y = 'q'), bquote(x <- as.character(x + y)), 'b', 'c')
  write_commit(store, c)

  c <- commit(list(x = 'r', y = 'q', z = 's'), bquote(z <- as.numeric(x)), 'c', 'd')
  write_commit(store, c)

  store
}


commit_memory_store <- function () fill_commits(storage::memory())

commit_filesystem_store <- function (where = tempdir()) fill_commits(empty_store(where))



sample_graph <- function (.data = TRUE)
{
  m <- commit_memory_store()
  graph(m, .data = .data)
}


expanded_graph <- function (.data = TRUE)
{
  store <- commit_memory_store()

  storage::os_write(store, 101,  auto_tags(101),  't')
  storage::os_write(store, 100L, auto_tags(100L), 'u')

  c <- commit(list(x = 'r', y = 'q', z = 's', w = 't'),
              bquote(w <- as.numeric(x) + 1), 'd', 'e')
  write_commit(store, c)

  c <- commit(list(x = 'u', y = 'q', z = 's', w = 't'), bquote(x <- 100L), 'e', 'f')
  write_commit(store, c)

  graph(store, .data = .data)
}


sample_steps <- function (.data = TRUE)
{
  graph_to_steps(sample_graph(.data))
}


empty_steps <- function ()
{
  structure(list(steps = list(), links = list()), class = 'steps')
}


add_commit <- function (store, objects, expr, parent_id, id)
{
  ids <- lapply(objects, function (o) {
    storage::os_write(store, o, auto_tags(o))
  })

  write_commit(store, commit(ids, expr, parent_id, id))
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


expect_connected <- function (steps, source, target, info = NULL)
{
  ids <- vapply(steps$steps, `[[`, character(1), i = 'object_id')
  source_id <- steps$steps[[match(source, ids)]]$id
  target_id <- steps$steps[[match(target, ids)]]$id

  cmp <- vapply(steps$links, identical, logical(1),
                y = list(source = source_id, target = target_id))

  expect(any(cmp), sprintf("there is no link from %s to %s", source, target),
         info = info)
}

