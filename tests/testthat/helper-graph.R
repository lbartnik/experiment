sample_memory_store <- function ()
{
  m <- storage::memory()

  c <- commit(list(x = 1), bquote(), NA_character_, 'a', list(x = 'p'))
  commit_store(c, m)

  c <- commit(list(x = 1, y = 2L), bquote(), 'a', 'b', list(x = 'p', y = 'q'))
  commit_store(c, m)

  c <- commit(list(x = '3', y = 2L), bquote(), 'b', 'c', list(x = 'r', y = 'q'))
  commit_store(c, m)

  c <- commit(list(x = '3', y = 2L, z = 1), bquote(), 'c', 'd', list(x = 'r', y = 'q', z = 's'))
  commit_store(c, m)

  m
}

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


expect_exists <- function (what, where, info = NULL)
{
  what_label  <- quasi_label(rlang::enquo(what))
  where_label <- quasi_label(rlang::enquo(where))

  cmp <- vapply(where, identical, logical(1), y = what)
  expect(any(cmp), sprintf("%s does not exist in %s", what_label$lab, where_label$lab),
         info = info)
}
