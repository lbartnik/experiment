#' Graph of commits.
#' 
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


find_first_parent <- function (g, id)
{
  g <- Filter(function (co) (id %in% co$object_ids), g)
  i <- which.min(vapply(g, function (co) co$level, numeric(1)))
  g[[i]]
}


#' @rdname graph
#' @export
#' @import htmlwidgets
#' @import dplyr
#' @importFrom magrittr %>%
#' 
#' @examples
#' plot(graph(modelling()))
#' 
plot.graph <- function (x, ...)
{
  node_color <- function (n)
  {
    if (identical(n$id, internal_state$last_commit)) return('red')
    if (is.na(n$parent)) return('green')
    '#0ff'
  }
  
  nodes <- lapply(x, function (n) list(id = n$id,
                                       label = storage::shorten(n$id),
                                       color = node_color(n),
                                       x     = n$level))
  vars  <- lapply(x, function (n) list(id = paste0(n$id, "objs"),
                                       label = paste(names(n$objects), collapse = ", "),
                                       color = "yellow"))
  nodes <-
    dplyr::bind_rows(c(nodes, vars)) %>%
    apply(1, as.list)

  edges <- lapply(x, function (n) {
    c(lapply(n$children, function (c) list(from = n$id, to = c)),
      list(list(from = n$id, to = paste0(n$id, "objs"))))
  }) %>%
    unlist(recursive = FALSE) %>%
    unname
  
  input <- list(
    data = list(
      nodes = nodes,
      edges = edges
    ),
    settings = list(autoResize = TRUE)
  )
  
  # create the widget
  htmlwidgets::createWidget("experiment", input, width = NULL, height = NULL)
}


#' @export
fullhistory <- function() graph(internal_state$stash)


