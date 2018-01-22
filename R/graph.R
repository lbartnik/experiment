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

  if (!length(commits)) return(commits)

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
#' @param x Object to be tested.
is_graph <- function (x) inherits(x, 'graph')


#' @rdname graph
#' @export
#' @importFrom graphics plot
#'
#' @param ... Extra parameters to control plotting.
#'
#' @examples
#' \dontrun{
#' plot(graph(fullhistory()))
#' }
#'
plot.graph <- function (x, ...)
{
  plot(graph_to_steps(x))
}


graph_leaves <- function (g)
{
  stopifnot(is_graph(g))
  Filter(function(node) isTRUE(length(node$children) == 0), g)
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
#' @rdname steps_internal
#'
#' @importFrom utils head tail
#' @importFrom defer defer_
#'
graph_to_steps <- function (graph)
{
  stopifnot(is_graph(graph))

  # convert each single commit
  all <- lapply(graph, function (commit) {
    new_objects <- introduced_in(graph, commit$id)
    if (!length(new_objects)) return(NULL)
    commit_to_steps(commit, new_objects)
  })
  out <- vapply(all, is.null, logical(1))
  all <- all[!out]

  steps <- unlist(lapply(all, `[[`, i = 'steps'), recursive = FALSE)
  links <- unlist(lapply(all, `[[`, i = 'links'), recursive = FALSE)

  find_parent <- function (id)
  {
    parent <- graph[[id]]$parent
    if (is.na(parent) || length(all[[parent]]$steps)) return(parent)
    return(find_parent(parent))
  }

  # connect the last object of each "parent" commit with the first
  # object of each of its "children"
  bridges <- unname(lapply(graph[!out], function (commit) {
    parent <- find_parent(commit$id)
    if (is.na(parent)) return(NULL)
    list(
      source = last(all[[parent]]$steps)$id,
      target = first(all[[commit$id]]$steps)$id
    )
  }))
  bridges <- bridges[!vapply(bridges, is.null, logical(1))]
  links <- c(links, bridges)

  # return the final "steps" structure
  structure(list(steps = unname(steps), links = unname(links)),
            class = 'steps')
}


#' @description `is_steps` verifies if the given object is a valid
#' `steps` structure.
#'
#' @rdname steps
#'
is_steps <- function (x) inherits(x, 'steps')


#' Interactive history.
#'
#' @description `plot.steps` open an interactive history viewer.
#' @param x The `steps` history object to be printed or viewed in `plot`; the object to be tested in `is_steps`.
#' @param ... Extra arguments for printing/plotting.
#'
#' @export
#' @rdname steps
#'
plot.steps <- function (x, ...) render_steps(x)


render_steps <- function (steps, options = list())
{
  stopifnot(is_steps(steps))

  processed <- plot_to_dependencies(steps$steps, is_knitr())
  steps$steps <- processed$steps

  # create the widget
  htmlwidgets::createWidget("experiment", list(data = steps, options = options),
                            dependencies = processed$html_deps)
}



#' @description `plot.steps` open an interactive history viewer.
#'
#' @export
#' @rdname steps
#'
print.steps <- function(x, ...)
{
  cat("A `steps` history object, contains ", length(x$steps), " step(s).\n")

  type <- vapply(x$steps, `[[`, character(1), i = 'type')
  cat(sum(type == 'object'), " object(s) and ", sum(type == 'plot'), " plot(s)\n")

  invisible(x)
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
#' @rdname steps_internal
#'
commit_to_steps <- function (commit, objects)
{
  # turns an object/plot into a step structure
  generate_step <- function(name, id, object) {
    ans <- list(
      id        = crc32(paste0(commit$id, id)),
      expr      = format_expression(commit$expr),
      commit_id = commit$id,
      object_id = id
    )

    if (identical(name, '.plot')) {
      c(ans, list(
        type = 'plot',
        contents = as.character(object)
      ))
    }
    else {
      c(ans, list(
        type      = "object",
        name      = name,
        desc      = description(object)
      ))
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
#' @rdname steps_internal
#'
introduced_in <- function (graph, id)
{
  c <- graph[[id]]
  if (is.na(c$parent)) return(names(c$objects))

  p <- graph[[c$parent]]
  new_objs <- Filter(function (n) {
    is.na(match(n, names(p$objects))) || !identical(c$object_ids[[n]], p$object_ids[[n]])
  }, setdiff(names(c$objects), '.plot'))

  # there is a plot (first condition) and it's different from
  # what was there before (second condition)
  if (!is.null(c$object_ids$.plot) &&
      !identical(c$object_ids$.plot, p$object_ids$.plot))
  {
    return(c(new_objs, '.plot'))
  }

  new_objs
}


#' @description `read_objects` reads every object/plot and fills in the
#' `contents` or `desc`ription. It is particularly useful when initial
#' `steps` graph has been read without objects' contents, e.g. in
#' [query_by].
#'
#' @param s `steps` object.
#' @param store An object store object, e.g. [storage::filesystem].
#'
#' @rdname steps_internal
#'
read_objects <- function (s, store)
{
  s$steps <- lapply(s$steps, function (step) {
    if (!is_empty(step$contents) || !is_empty(step$desc)) return(step)
    obj <- storage::os_read_object(store, step$object_id)

    if (identical(step$type, 'object')) {
      step$desc <- description(obj)
    }
    else {
      step$contents <- as.character(obj)
    }

    step
  })

  s
}


#' @description `find_root_id` searches for the single commit
#' in the graph without a parent.
#'
#' @param g A [graph] object.
#'
#' @rdname steps_internal
#'
find_root_id <- function (g)
{
  stopifnot(is_graph(g))
  root <- names(Filter(function (co) is.na(co$parent), g))

  stopifnot(length(root) == 1)
  root
}


#' @description `format` prepares the expression for display
#' in a HTML page.
#'
#' @param code R `expression`.
#'
#' @rdname steps_internal
#'
format_expression <- function (code)
{
  return(paste(deparse(code), sep = '\n'))

  form <- tryCatch(formatR::tidy_source(text = code, blank = FALSE, comment = FALSE,
                                        width.cutoff = 72,
                                        output = FALSE),
                   error = function(e) 'error')
  if (!identical(form, 'error')) return(form$text.tidy)
}


#' @description `description` format the expression for display
#' in a HTML page.
#'
#' @param object Object to be described.
#'
#' @import broom
#' @rdname steps_internal
#'
description <- function (object)
{
  if (is_empty(object)) return(NA_character_)

  if (is.data.frame(object)) return(paste0('data.frame [', nrow(object), ', ', ncol(object), ']'))

  if (inherits(object, 'lm')) {
    g <- glance(object)
    return(paste('adj R2:', format(g$adj.r.squared, digits = 2),
                 'AIC: ', format(g$AIC, digits = 2),
                 'df: ', g$df))
  }

  paste(class(object), collapse = '.')
}


#' Compare two SVG images.
#'
#' SVG images are processed in this package as base64-encoded, XML text
#' data. When produced, certain differences are introduced in XML
#' attributes that have no effect on the final plots. That is why,
#' however, SVG plots need to be compared graphically and not textually.
#' This function produces a thumbnail of each SVG image and then
#' compares the raster graphic.
#'
#' @param a First SVG image.
#' @param b Second SVG image.
#' @return `TRUE` if SVGs are the same plot-wise.
#'
#' @import rsvg
#' @import jsonlite
#'
svg_equal <- function (a, b)
{
  if (is_empty(a)) return(is_empty(b))
  if (is_empty(b)) return(FALSE)

  a <- try(rsvg(base64_dec(a), 100, 100), silent = TRUE)
  b <- try(rsvg(base64_dec(b), 100, 100), silent = TRUE)
  if (is_error(a) && is_error(b)) return(TRUE)

  isTRUE(all.equal(a, b))
}


#' @description `plot_to_dependencies` processes the `steps` list and
#' either:
#' * extracts plots into `png`` files, which are then attached to the
#'   html widget via `dependencies` (see [htmltools::htmlDependency])
#' * embeds the png bitmap as base64-encoded string under the `contents`
#'   key
#'
#' @param embed Whether to embed plots as base64-encoded strings.
#' @param steps A list of `step`s.
#'
#' @rdname steps_internal
plot_to_dependencies <- function (steps, embed = is_knitr())
{
  html_dir <- file.path(tempdir(), 'experiment-html-deps')
  dir.create(html_dir, showWarnings = FALSE)

  steps <- lapply(steps, function (step) {
    if (!identical(step$type, "plot")) return(step)
    path <- file.path(html_dir, paste0(step$id, '.png'))
    rsvg::rsvg_png(jsonlite::base64_dec(step$contents), path, height = 300)

    step$contents <- if (isTRUE(embed))
      jsonlite::base64_enc(readBin(path, 'raw', file.info(path)$size))

    step
  })

  if (isTRUE(embed)) {
    deps <- list()
  }
  else {
    plots <- Filter(function (step) identical(step$type, 'plot'), steps)
    ids   <- vapply(plots, `[[`, character(1), i = 'id')
    plots <- vapply(plots, function(plot) paste0(plot$id, '.png'), character(1))
    names(plots) <- ids

    deps <-
      list(
        htmltools::htmlDependency(
          "plots",
          version = "1",
          src = html_dir,
          attachment = plots
        )
      )
  }

  list(steps = steps, html_deps = deps)
}


step_by <- function (steps, id, object_id)
{
  stopifnot(is_steps(steps))
  if (missing(id) && missing(object_id)) {
    stop('no query provided')
  }

  find_by <- function (value) {
    name <- deparse(substitute(value))
    (vapply(steps$steps, `[[`, character(1), i = name) == value)
  }

  if (!missing(object_id)) {
    i <- find_by(object_id)
  }
  else {
    i <- find_by(id)
  }

  # TODO this actually doesn't have to hold, it's possible that the same
  # object appears in a number of commits and gets promoted as a step
  stopifnot(sum(i) == 1)
  nth(steps$steps, which(i))
}


count <- function (x)
{
  stopifnot(is_steps(x))
  length(x$steps)
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
  if (!length(i)) return(NULL)
  g[[i]]
}
