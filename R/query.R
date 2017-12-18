
#' @import lazyeval
#' @import storage
#'
query_by <- function (..., .related = "plots")
{
  dots <- lazyeval::lazy_dots(...)

  g <- graph(internal_state$stash)
  s <- graph_to_steps(g)

  s <- reduce_steps(s, dots, internal_state$stash)
}



reduce_steps <- function (s, dots, store)
{
  stopifnot(is_steps(s))
  stopifnot(is_lazy_dots(dots))

  parent_env <- parent.frame(1)

  matching <- vapply(s$steps, verify_step, logical(1),
                     dots = dots, parent_env = parent_env, store = store)

  ids <- vapply(s$steps, `[[`, character(1), i = 'id')

  for (id in ids[!matching]) {
    s <- remove_step(s, id)
  }

  s
}


remove_step <- function (s, id)
{
  # for an object that doesn't match, remove it from steps and
  # "merge" links by connecting its children to its parent
  target <- (vapply(s$links, `[[`, character(1), i = 'target') == id)
  source <- (vapply(s$links, `[[`, character(1), i = 'source') == id)

  stopifnot(sum(target) <= 1)

  # move children "up"
  parent <- if (sum(target)) s$links[target][[1]]$source else NA_character_
  s$links[source] <- lapply(s$links[source], function (link) {
    link$source <- parent
    link
  })

  # remove "dangling" parent
  if (sum(target))
    s$links[target] <- NULL

  # once edges are updated, remove nodes
  s$steps[vapply(s$steps, `[[`, character(1), i = 'id') == id] <- NULL

  s
}


verify_step <- function (step, dots, parent_env, store)
{
  # prepare the search verbs
  search_env <- new.env(parent = parent_env)
  search_funs <- list(
    inherits = function(...) {
      classes <- as.character(list(...))
      any(class %in% classes)
    },
    name = function(...) {
      names <- as.character(list(...))
      any(name %in% names)
    }
  )
  search_funs <- lapply(search_funs, `environment<-`, value = search_env)

  # prepare the hierarchy of environments in which lazy dots will be evaluated
  tags <- storage::os_read_tags(store, step$id)
  data <- as.environment(c(search_funs, tags))
  parent.env(data) <- parent_env
  parent.env(search_env) <- data

  # evaluate all lazy dots in the bottom-most environment in that hierarchy
  ans <- lapply(dots, function (ldot) tryCatch(lazyeval::lazy_eval(ldot, data = data),
                                                 error = function(e) NA_character_))

  # all must match
  all(unlist(ans))
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

