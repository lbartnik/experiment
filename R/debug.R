stash <- function () internal_state$stash

show_commits <- function (simple = TRUE)
{
  lazy_tags <- lazyeval::as.lazy_dots(list(quote(class == 'commit')))
  ids <- storage::os_find(internal_state$stash, lazy_tags)
  lapply(ids, function (id) {
    co <- commit_restore(id, internal_state$stash)
    print(co, simple = simple)
  })

  invisible()
}
