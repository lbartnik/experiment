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


is_graph <- function (x) inherits(x, 'graph')


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
                                       short_id = storage::shorten(n$id),
                                       label = paste(names(n$objects), collapse = ", "),
                                       color = node_color(n),
                                       x     = n$level))
  nodes <- lapply(nodes, function (n) {
    n$title <- paste(n$short_id, ':', n$label)
    n
  })

  edges <- lapply(x, function (n) {
    lapply(n$children, function (c) list(from = n$id, to = c))
  })
  edges <- unlist(edges, recursive = FALSE)

  input <- list(
    data = list(
      nodes = unname(nodes),
      edges = unname(edges)
    ),
    settings = list(
      autoResize = TRUE,
      interaction = list (
        dragNodes = FALSE,
        hover = TRUE
      )
    )
  )

  # create the widget
  htmlwidgets::createWidget("experiment", input, width = NULL, height = NULL)
}


#' @export
fullhistory <- function() graph(internal_state$stash)


