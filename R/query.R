
#' Query the current history.
#'
#' @description `query_by` accepts an arbitrary list of expressions and
#' returns objects and plots for which all evaluate to `TRUE`.
#'
#' @details The following helper functions can be use in expressions
#' to define conditions:
#' * `is_named(...)` name matches any from the list
#' * `inherits` object inherits from any of the specified classes
#'
#' The following variables can be used in expressions when defining
#' conditions:
#' * `name` object name
#' * `class` object class
#' * `id` object identifier`
#'
#' @param ... Search conditions.
#' @param .related Included related entities (objects or plots).
#'
#' @return `query_by` return a history graph reduced according to
#' conditions specified in the call.
#'
#' @export
#' @import lazyeval
#'
#' @rdname query
#'
#' @examples
#' \dontrun{
#' # search for a specific class
#' query_by(inherits("lm", "data.frame"))
#' query_by(lm %in% class || "data.frame" %in% class)
#'
#' # search for a specific name
#' query_by(is_named("input", "x", "model"))
#' query_by(name == "input" || name == "x" || name == "model")
#' }
#'
query_by <- function (..., .related = "plots")
{
  dots <- lazyeval::lazy_dots(...)

  g <- graph(internal_state$stash)
  s <- graph_to_steps(g)

  s <- reduce_steps(s, dots, internal_state$stash)

  s
}


#' @description `fullhistory` is an equivalent to calling `query_by`
#' without any conditions.
#'
#' @return `fullhistory` returns the full history graph.
#'
#' @export
#' @rdname query
#'
fullhistory <- function() graph_to_steps(graph(internal_state$stash, TRUE))


#' Operations on steps.
#'
#' @description `reduce_steps` reduces the graph of steps according to
#' conditions specified in `dots`.
#'
#' @param s Graph of steps, see [experiment::graph_to_steps].
#' @param dots Keep nodes that meet these conditions, see [lazyeval::dots].
#' @param store Object store to read tags from [storage::os_read_tags].
#' @return `reduce_steps` returns a reduced steps graph derived from `s`.
#'
#' @rdname steps_internal
#'
reduce_steps <- function (s, dots, store)
{
  stopifnot(is_steps(s))
  stopifnot(is_lazy_dots(dots))

  parent_env <- parent.frame(1)

  matching <- vapply(s$steps, verify_step, logical(1),
                     dots = dots, parent_env = parent_env, store = store)

  ids <- vapply(s$steps, `[[`, character(1), i = 'id')

  # remove all nodes that do not match the criteria
  for (id in ids[!matching]) {
    s <- remove_step(s, id)
  }

  s
}


#' @description `remove_step` removes a single step from graph of steps
#' `s` and updates links accordingly.
#'
#' @param id Identifier of object to be removed.
#' @return `remove_step` returns a reduced steps graph derived from `s`.
#'
#' @rdname steps_internal
#'
remove_step <- function (s, id)
{
  stopifnot(is_steps(s))

  # for an object that doesn't match, remove it from steps and
  # "merge" links by connecting its children to its parent
  target <- (vapply(s$links, `[[`, character(1), i = 'target') == id)
  source <- (vapply(s$links, `[[`, character(1), i = 'source') == id)

  stopifnot(sum(target) <= 1)

  # if thsi node has a parent, move its children "up"
  if (sum(target)) {
    parent <- s$links[target][[1]]$source
    s$links[source] <- lapply(s$links[source], function (link) {
      link$source <- parent
      link
    })

    # remove "dangling" parent
    s$links[target] <- NULL
  }
  # otherwise, remove its links altogether
  else {
    s$links[source] <- NULL
  }

  # once edges are updated, remove nodes
  s$steps[vapply(s$steps, `[[`, character(1), i = 'id') == id] <- NULL

  s
}


#' @description `verify_step` checks whether `step` meets the condition
#' defined in `dots`. It evaluates `dots` in an environment that has
#' `parent_env` as its parent. Object tags are read from `store`.
#'
#' @param step Step to be verified.
#' @param parent_env Evaluate `dots` in environment descending from this one.
#' @return `verify_step` returns `TRUE` if `step` meets the criteria and
#'         `FALSE` otherwise.
#'
#' @rdname steps_internal
#'
#' @import storage
#' @import lazyeval
#'
verify_step <- function (step, dots, parent_env, store)
{
  stopifnot(is_lazy_dots(dots))
  stopifnot(is.environment(parent_env))
  stopifnot(storage::is_object_store(store))

  # prepare the hierarchy of environments in which lazy dots will be evaluated
  tags <- storage::os_read_tags(store, step$id)
  data_env <- as.environment(c(tags, step))
  parent.env(data_env) <- parent_env

  # prepare the search verbs; functions' environment is data_env
  # and they belong to a search environment, which is also a child
  # of data_env
  dots_env <- search_funs(data_env)

  # evaluate all lazy dots in the bottom-most environment in that hierarchy
  ans <- lapply(dots, function (ldot) tryCatch(lazyeval::lazy_eval(ldot, data = dots_env),
                                                 error = function(e) NA_character_))

  # all must match
  all(unlist(ans))
}


search_funs <- function (data_env)
{
  search_funs <- list(
    inherits = function(...) {
      classes <- as.character(list(...))
      as.logical(length(intersect(class, classes)) > 0)
    },
    is_named = function(...) {
      names <- as.character(list(...))
      any(name %in% names)
    }
  )

  search_funs <- lapply(search_funs, `environment<-`, value = data_env)

  env <- as.environment(search_funs)
  parent.env(env) <- data_env

  env
}



# --- querying ---

#' @export
stashed <- function (..., ids)
{
  if (missing(ids)) {
    dots <- lazyeval::lazy_dots(...)
    ids <- storage::os_find(internal_state$stash, dots)
  }

  objs <- lapply(ids, storage::os_read_object, store = internal_state$stash)
  names(objs) <- ids

  objs
}



#' @export

#' @export
query_by_class <- function (value) results(stashed(class == value))


#' @export
query_by_name <- function (value)
{
  cmts <- stashed(class == 'commit')
  ids <- lapply(cmts, function (co) {
    m <- match(value, names(co$objects))
    if (!is.na(m)) return(co$objects[[m]])
    NULL
  })
  ids <- unique(unlist(ids))
  results(stashed(ids = ids))
}


#' @export
#' @import storage
#' @import crayon
explain <- function (id)
{
  stopifnot(is_nonempty_character(id))
  id <- to_long(id, internal_state$stash)

  g <- graph(internal_state$stash)
  c <- find_first_parent(g, id)

  explain_parents(g, id)

  t <- storage::os_read_tags(internal_state$stash, id)
  cat("in commit", crayon::yellow(storage::shorten(c$id)), ": ", crayon::green(format(c$expr)))
}


#' @importFrom defer defer extract_variables
explain_parents <- function (graph, id)
{
  stopifnot(is_graph(graph))
  c <- find_first_parent(graph, id)
  if (is.na(c$parent)) return()

  p <- commit_restore(c$parent, internal_state$stash, .data = FALSE)

  f <- function(){}; body(f) <- c$expr

  d <- defer_(f, .caller_env = as.environment(p$object_ids), .extract = T)
  v <- extract_variables(d)

  if (length(v)) {
    lapply(as.character(v), function (id) explain_parents(graph, id))
  }

  cat(format(c$expr), '\n')

  invisible()
}

