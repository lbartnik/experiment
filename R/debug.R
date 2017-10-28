stash <- function () internal_state$stash

show_commits <- function () {
  lazy_tags <- lazyeval::as.lazy_dots(list(quote(class == 'commit')))
  os_find(internal_state$stash, lazy_tags)
}
