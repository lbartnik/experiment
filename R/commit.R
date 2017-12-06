
#' Creates a new commit object.
#'
#'
commit <- function (contents, expression, parent, id, object_ids)
{
  objects <- as.list(contents)
  stopifnot(all_named(objects))

  if (missing(parent)) parent <- NA_character_
  if (missing(id)) id <- NA_character_
  if (missing(object_ids)) object_ids <- lapply(contents, function (x) NA_character_)

  structure(list(id = id, objects = objects, object_ids = object_ids,
                 expr = expression, parent = parent),
            class = 'commit')
}


is_commit <- function (x) inherits(x, 'commit')


plot_commit <- function (plots, expression, parent)
{
  plots <- as.list(plots)

  co <- commit(plots, expression, parent)
  class(co) <- c("plot", class(co))

  co
}


`parent<-` <- function (co, value)
{
  co$parent <- value
  co
}

commit_equal <- function (a, b)
{
  stopifnot(is_commit(a), is_commit(b))
  setequal(a$objects, b$objects)
}



#' Write commit to an object store.
#'
commit_store <- function (commit, store)
{
  stopifnot(is_commit(commit))
  stopifnot(storage::is_object_store(store))

  # name -> ID in object store
  objects <- lapply(commit$objects, function (o) {
    o  <- cleanup_object(o)
    id <- storage::compute_id(o)
    if (storage::os_exists(store, id)) return(id)

    tg <- auto_tags(o)
    storage::os_write(store, o, id = id, tags = tg)
  })

  if (is.na(commit$id))
  {
    commit$id <- storage::compute_id(commit)
  }

  # this should never happen because hash is computed from
  # both objects and parent id; if it does happen, something
  # is seriously broken
  if (storage::os_exists(store, commit$id))
  {
    stop("commit already exists, aborting", call. = FALSE)
  }

  # store list of object pointers + basic 'history' tags
  id <- storage::os_write(store, list(objects = objects, expr = commit$expr),
                          tags = list(class = 'commit', parent = commit$parent),
                          id = commit$id)

  commit
}



commit_restore <- function (id, store, .data = TRUE)
{
  stopifnot(is_nonempty_character(id),
            storage::is_object_store(store))

  co <- storage::os_read(store, id)
  objects <- lapply(co$object$objects, function (id) NA_character_)

  co <- commit(objects, co$object$expr, co$tags$parent, id, co$object$objects)
  if (isTRUE(.data)) {
    co <- commit_restore_data(co, store)
  }

  co
}


commit_restore_data <- function (co, store)
{
  co$objects <- lapply(co$object_ids, function (id) storage::os_read_object(store, id))
  co
}



# TODO could be turned into a S3 method
auto_tags <- function (obj)
{
  list(class = class(obj), time = Sys.time())
}


#' Removes references to environments.
#'
#' Some objects (e.g. formula, lm) store references to environments
#' in which they were created. This function replaces each such reference
#' with a reference to `emptyenv()`.
#'
#' @param obj Object to be processed.
#' @return `obj` with environment references replaced by `emptyenv()`
#'
cleanup_object <- function (obj)
{
  if (is.environment(obj)) return(emptyenv())

  attrs <- lapply(attributes(obj), cleanup_object)

  if (is.list(obj))
  {
    obj <- lapply(obj, cleanup_object)
  }

  attributes(obj) <- attrs
  obj
}




#' @export
#' @rdname commit
print.commit <- function (x, simple = FALSE, ...)
{
  if (isTRUE(simple))
  {
    cat(substr(x$id, 1, 8), ': ', names(x$objects), '\n')
  }
  else
  {
    tag_print <- function (x) {
      if (!length(x)) return('')
      if (length(x) < 2) return(as.character(x))
      paste0('[', paste(x, collapse = ', '), ']')
    }

    cat('Commit : ', ifelse(is.na(x$id), '<no id>', x$id), '\n')
    cat('objects :\n')
    mapply(function (name, id) {
      tags <- storage::os_read_tags(internal_state$stash, id)
      tags <- vapply(tags, tag_print, character(1))
      cat('  ', name, ': ', paste(names(tags), '=', tags, collapse = ', '), '\n')
    }, name = names(x$objects), id = as.character(x$object_ids))
    cat('\n')
  }
}


#' @export
`.DollarNames.commit` <- function (x, pattern = "")
{
  grep(pattern, c("restore", names(x)), value = TRUE)
}


#' @export
`$.commit` <- function (x, i = "")
{
  if (isTRUE(i %in% names(x))) {
    return(x[[i]])
  }

  # RStudio calls the operator so we cannot restore directly, but only
  # after the user confirms the command and its return value is printed
  if (identical(i, "restore")) {
    return(structure(list(commit = x), class = 'restorer'))
  }

  stop("unknown option: ", i, call. = FALSE)
}


#' @export
print.restorer <- function (x)
{
  co <- commit_restore_data(x$commit, internal_state$stash)
  restore_historical_commit(co)
  message(crayon::green('Commit restored'))
  print(co)
}


#' @import storage
to_long <- function (x, store = internal_state$stash)
{
  stopifnot(is.character(x), length(x) > 0)
  storage::enlongate(x, store)
}
