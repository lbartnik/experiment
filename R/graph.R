graph <- function (store)
{
  # read all commits
  ids <- storage::os_find(store, lazy_dots(class == 'commit'))
  
  cmts <- lapply(ids, function (commit_id) 
    commit_restore(commit_id, internal_state$stash, .data = FALSE))
  names(cmts) <- vapply(cmts, `[[`, character(1), i = 'id')

  # identify children and levels; start with root
  root <- names(Filter(function (co) is.na(co$parent), cmts))
  cmts <- children(cmts, root, 1)
  
  structure(cmts, class = 'graph')
}


children <- function (commits, id, level)
{
  found <- names(Filter(function (co) co$parent == id, commits))
  commits[[id]]$children <- found
  commits[[id]]$level <- level

  for (id in found) {
    commits <- children(commits, id, level + 1)
  }
  
  commits
}


