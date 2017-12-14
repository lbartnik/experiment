#' Graph of commits.
#'
#' @export
#' @import storage
graph <- function (store, .data = FALSE)
{
  # read all commits
  ids <- storage::os_find(store, lazyeval::lazy_dots(class == 'commit'))

  commits <- lapply(ids, function (commit_id)
    commit_restore(commit_id, store, .data = .data))
  names(commits) <- ids

  # identify children and levels; start with root
  assign_children <- function (commits, id, level)
  {
    found <- names(Filter(function (co) co$parent == id, commits))
    commits[[id]]$children <- found
    commits[[id]]$level <- level

    for (id in found) {
      commits <- assign_children(commits, id, level + 1)
    }

    commits
  }

  root <- names(Filter(function (co) is.na(co$parent), commits))
  structure(assign_children(commits, root, 1), class = 'graph')
}


is_graph <- function (x) inherits(x, 'graph')


graph2steps <- function (graph)
{
  stopifnot(is_graph(graph))

  root <- names(Filter(function (co) is.na(co$parent), graph))
  processCommit(graph, root)
}

commit2steps <- function (commit, filter)
{
  filter <- names(commit$objects) %in% filter

  names <- names(commit$objects)[filter]
  ids <- as.character(commit$object_ids)[filter]
  objects <- commit$objects[filter]

  steps <- mapply(function(name, id, object) {
    if (identical(name, '.plot')) {
      list(
        type = 'plot',
        id   = id,
        expr = format(commit$expr),
        contents = as.character(object)
      )
    }
    else {
      list(
        name = name,
        type = "object",
        id   = id,
        expr = format(commit$expr)
      )
    }
  },
  name = names,
  id   = ids,
  object = objects,
  SIMPLIFY = FALSE,
  USE.NAMES = FALSE
  ) # steps <- mapply

  links <- mapply(function (source, target) list(source = source, target = target),
                  source = head(ids, -1), target = tail(ids, -1),
                  SIMPLIFY = FALSE, USE.NAMES = FALSE)

  list(steps = steps, links = links)
}

new_objects <- function (graph, id)
{
  c <- graph[[id]]
  if (is.na(c$parent)) return(names(c$objects))

  p <- graph[[c$parent]]
  Filter(function (n) {
    is.na(match(n, names(p$objects))) || !identical(c$object_ids[[n]], p$object_ids[[n]])
  }, names(c$objects))
}

processCommit <- function (graph, id)
{
  parent <- commit2steps(graph[[id]], new_objects(graph, id))
  children <- lapply(graph[[id]]$children, function (child) processCommit(graph, child))

  bridge <- lapply(children, function (child) list(
    source = tail(parent$steps, 1)[[1]]$id,
    target = head(child$steps, 1)[[1]]$id
  ))

  links <- c(parent$links, bridge,
             unlist(lapply(children, `[[`, i = 'links'), recursive = FALSE))
  steps <- c(parent$steps,
             unlist(lapply(children, `[[`, i = 'steps'), recursive = FALSE))

  list(steps = steps, links = links)
}






steps <- function ()
{
  svgPlot <- system.file("examples/plot.svg", package = "experiment")

  steps <- list(
    list(
      id = "1",
      type = "object",
      name = "input",
      expr = format(bquote(input <-
                             system.file("extdata/block_62.csv", package = "experiment") %>%
                             readr::read_csv(na = 'Null') %>%
                             rename(meter = LCLid, timestamp = tstp, usage = `energy(kWh/hh)`) %>%
                             filter(meter %in% c("MAC004929", "MAC000010", "MAC004391"),
                                    year(timestamp) == 2013)
      ))
    ),
    list(
      id = "2",
      type = "object",
      name = "input",
      expr = format(bquote(input %<>%
                             mutate(timestamp = floor_date(timestamp, 'hours')) %>%
                             group_by(meter, timestamp) %>%
                             summarise(usage = sum(usage))
      ))
    ),
    list(
      id = "3",
      type = "plot",
      contents = jsonlite::base64_enc(readBin(svgPlot, "raw", n = file.size(svgPlot))),
      expr = format(bquote(
        with(filter(input, meter == "MAC004929"),
             plot(timestamp, usage, type = 'p', pch = '.'))
      ))
    )
  )

  links <- list(
    list(target = 2, source = 1),
    list(target = 3, source = 2)
  )

  jsonlite::toJSON(list(steps = steps, links = links),
                   pretty = TRUE, auto_unbox = TRUE)
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
  input <- list(data = graph2steps(x))
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


#' @import formatR
format <- function (code)
{
  return(paste(deparse(code), sep = '\n'))

  form <- tryCatch(formatR::tidy_source(text = code, blank = FALSE, comment = FALSE,
                                        width.cutoff = 72,
                                        output = FALSE),
                   error = function(e) 'error')
  if (!identical(form, 'error')) return(form$text.tidy)
}
