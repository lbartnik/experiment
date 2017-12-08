#' Graph of commits.
#'
#' @export
#' @import storage
graph <- function (store, .data = FALSE)
{
  # read all commits
  ids <- storage::os_find(store, lazyeval::lazy_dots(class == 'commit'))

  cmts <- lapply(ids, function (commit_id)
    commit_restore(commit_id, store, .data = .data))
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
  input <- list(data = graph_js(x))
  # create the widget
  htmlwidgets::createWidget("experiment", input, width = NULL, height = NULL)
}


#' @export
fullhistory <- function() graph(internal_state$stash, TRUE)


#' @import storage
graph_js <- function (x)
{
  # nodes: commits
  commits <- lapply(x, function (n) {
    list(id     = n$id,
         label  = storage::shorten(n$id),
         rclass = "commit")
  })
  commits <- unname(commits)

  # nodes: objects
  objects <- lapply(x, function (n) {
    mapply(function (obj, id, name) {
      list(id     = paste0(id, ':', name),
           rclass = class(obj)[[1]],
           label  = name)
    }, obj = n$objects, id = n$object_ids, name = names(n$objects), SIMPLIFY = FALSE)
  })
  objects <- unname(unlist(objects, recursive = FALSE))

  # links
  links <- lapply(x, function (n) {
    children <- lapply(n$children, function (c) list(source = n$id, target = c))
    objects  <- lapply(paste0(n$object_ids, ':', names(n$objects)),
                       function (target) list(source = n$id, target = target))
    c(children, objects)
  })
  links <- unname(unlist(links, recursive = FALSE))

  jsonlite::toJSON(list(commits = commits, objects = objects, links = links),
                   pretty = FALSE, auto_unbox = TRUE)
}


steps <- function ()
{
  svgPlot <- system.file("examples/plot.svg", package = "experiment")

  data <- list(
    list(
      id = "1",
      type = "object",
      name = "input",
      expr = paste(deparse(expression(input <-
          readr::read_csv("halfhourly/block_62.csv", na = 'Null') %>%
          rename(meter = LCLid, timestamp = tstp, usage = `energy(kWh/hh)`) %>%
          filter(meter %in% c("MAC004929", "MAC000010", "MAC004391"),
                 year(timestamp) == 2013)
      )), sep = "\n")
    ),
    list(
      id = "2",
      type = "object",
      name = "input",
      expr = paste(deparse(expression(input %<>%
        mutate(timestamp = floor_date(timestamp, 'hours')) %>%
        group_by(meter, timestamp) %>%
        summarise(usage = sum(usage))
      )), sep = "\n")
    ),
    list(
      id = "3",
      type = "plot",
      contents = jsonlite::base64_enc(readBin(svgPlot, "raw", n = file.size(svgPlot))),
      expr = paste(deparse(expression(
        with(filter(input, meter == "MAC004929"),
             plot(timestamp, usage, type = 'p', pch = '.'))
      )), "\n")
    )
  )

  links <- list(
    list(target = 2, source = 1),
    list(target = 3, source = 2)
  )

  jsonlite::toJSON(list(data = data, links = links),
                   pretty = TRUE, auto_unbox = TRUE)
}


