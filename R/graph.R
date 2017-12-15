#' Graph of all commits in `store`.
#'
#' Reads all commits from `store` and assigns the `children` and
#' `level` attributes. `children` is a `character` vector containing
#' identifiers of children commit, `level` is the distance from the
#' root of the tree.
#'
#' @param store A data store, e.g. `storage::filesystem`.
#' @param .data Wether to read full object data.
#' @return An object of S3 class `graph`.
#'
#' @export
#' @import storage
#' @rdname graph
#'
graph <- function (store, .data = FALSE)
{
  # read all commits
  ids <- storage::os_find(store, lazyeval::lazy_dots(class == 'commit'))

  commits <- lapply(ids, function (commit_id)
    commit_restore(commit_id, store, .data = .data))
  names(commits) <- ids

  commits <- structure(commits, class = 'graph')
  assign_children(commits, find_root_id(commits), 1)
}

# identify children and levels; start with root
# used only inside graph()
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


#' @rdname graph
is_graph <- function (x) inherits(x, 'graph')


#' @rdname graph
#' @export
#' @import htmlwidgets
#'
#' @examples
#' plot(graph(modelling()))
#'
plot.graph <- function (x, ...)
{
  input <- list(data = graph_to_steps(x))
  # create the widget
  htmlwidgets::createWidget("experiment", input, width = NULL, height = NULL)
}



#' Transform a graph of commits into a graph of steps.
#'
#' A step is an introduction of a new object or a new plot to the
#' session. Graph of steps is easier to read for humans than a graph of
#' commits becase only the relevant (new) information is shown in each
#' node of the graph. Thus, translating from commits to steps is the
#' first step to visualize the history stored in commits.
#'
#' `graph_to_steps` is the main function that orchestrates the
#' transformation.
#'
#' @param graph Object returned by [graph()].
#' @return Object of S3 class `steps`.
#'
#' @rdname steps
#'
graph_to_steps <- function (graph)
{
  stopifnot(is_graph(graph))

  # convert each single commit
  all <- lapply(graph, function (commit) {
    new_objects <- introduced_in(graph, commit$id)
    commit_to_steps(commit, new_objects)
  })

  steps <- unlist(lapply(all, `[[`, i = 'steps'), recursive = FALSE)
  links <- unlist(lapply(all, `[[`, i = 'links'), recursive = FALSE)

  # connect the last object of each "parent" commit with the first
  # object of each of its "children"
  bridges <- lapply(graph, function (commit) {
    lapply(commit$children, function (child) {
      list(
        source = last(all[[commit$id]]$steps)$id,
        target = first(all[[child]]$steps)$id
      )
    })
  })
  bridges <- unlist(bridges, recursive = FALSE)
  links <- c(links, bridges)

  # return the final "steps" structure
  structure(list(steps = unname(steps), links = unname(links)),
            class = 'steps')
}



#' @description `commit_to_steps` generates a `list` with two elements:
#' * `steps` with a separate entry for each variable/plot that matches
#'   the `objects` filter
#' * `links` which defines links (graph edges) between `steps`
#'
#' @param commit A [commit()] object.
#' @param objects Filter for objects present in the commit.
#' @return `commit_to_steps` returns a `list` of `steps` and `links`.
#'
#' @rdname steps
#'
commit_to_steps <- function (commit, objects)
{
  # turns an object/plot into a step structure
  generate_step <- function(name, id, object) {
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
  }

  # define the TRUE/FALSE filter
  filter <- names(commit$objects) %in% objects

  names <- names(commit$objects)[filter]
  ids <- as.character(commit$object_ids)[filter]
  objects <- commit$objects[filter]

  # get all steps
  steps <- mapply(generate_step, name = names, id = ids, object = objects,
                  SIMPLIFY = FALSE, USE.NAMES = FALSE)

  # get links between these teps
  links <- mapply(function (source, target) list(source = source, target = target),
                  source = head(ids, -1), target = tail(ids, -1),
                  SIMPLIFY = FALSE, USE.NAMES = FALSE)

  list(steps = steps, links = links)
}


#' @description `introduced_in` generates the filter for
#' `commit_to_steps`'s `objects` parameter. It does it by comparing the
#' contents of the commit `id` against the contents of its parent.
#'
#' @param id Identifier of a commit in `graph`.
#' @return `introduced_in` returns a `character` vector.
#'
#' @rdname steps
#'
introduced_in <- function (graph, id)
{
  c <- graph[[id]]
  if (is.na(c$parent)) return(names(c$objects))

  p <- graph[[c$parent]]
  Filter(function (n) {
    is.na(match(n, names(p$objects))) || !identical(c$object_ids[[n]], p$object_ids[[n]])
  }, names(c$objects))
}


find_root_id <- function (g)
{
  stopifnot(is_graph(g))
  root <- names(Filter(function (co) is.na(co$parent), g))

  stopifnot(length(root) == 1)
  root
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





#' @import jsonlite
graph_to_json <- function (g)
{
  stopifnot(is_graph(g))
  steps <- graph_to_steps(g)
  jsonlite::toJSON(steps, pretty = TRUE, auto_unbox = TRUE)
}

find_first_parent <- function (g, id)
{
  g <- Filter(function (co) (id %in% co$object_ids), g)
  i <- which.min(vapply(g, function (co) co$level, numeric(1)))
  g[[i]]
}

#' @export
fullhistory <- function() graph(internal_state$stash, TRUE)
