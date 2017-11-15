#' @export
#' @import storage
graph <- function (store)
{
  # read all commits
  ids <- storage::os_find(store, lazyeval::lazy_dots(class == 'commit'))
  
  cmts <- lapply(ids, function (commit_id) 
    commit_restore(commit_id, store, .data = FALSE))
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


#' @export
#' @import htmlwidgets
#' 
plot.graph <- function (x, ...)
{
  x <- unname(x)

  nodes <- lapply(x, function (n) {
    commit <- list(id = n$id, label = storage::shorten(n$id), color = '#0ff')
    variables <- list(id = paste0(n$id, "objs"),
                      label = paste(names(n$objects), collapse = ", "),
                      color = "yellow")
    c(list(commit), list(variables))
  })
  nodes <- unlist(nodes, recursive = FALSE)

  edges <- lapply(x, function (n) {
    c(lapply(n$children, function (c) list(from = n$id, to = c)),
      list(list(from = n$id, to = paste0(n$id, "objs"))))
  })
  edges <- unlist(edges, recursive = FALSE)
  
  x <- list(
    data = list(
      nodes = nodes,
      edges = edges
    ),
    settings = list(autoResize = TRUE)
  )
  
  # create the widget
  htmlwidgets::createWidget("experiment", x, width = NULL, height = NULL)
}


#' @export
fullhistory <- function() graph(internal_state$stash)


