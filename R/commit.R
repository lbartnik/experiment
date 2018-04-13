
#' Creates a new commit object.
#'
#' @param artifacts A named `list` of object ids.
#' @param expression Expression associated with this commit.
#' @param parent Identifier of the parent commit.
#' @param id Identifier of this commit.
#'
#' @rdname commit
#'
commit <- function (artifacts, expression, parent, id = NA_character_)
{
  stopifnot(is.list(artifacts), all_named(artifacts))

  if (missing(id)) id <- NA_character_
  tags <- lapply(artifacts, function (x) list())

  structure(list(id = id, object_ids = artifacts, objects = list(),
                 tags = tags, expr = expression, parent = parent),
            class = 'commit')
}


#' @rdname commit
#' @param x Object to be tested or printed.
is_commit <- function (x) inherits(x, 'commit')


# it has to be optimizd in case commits are bulky
commit_equal <- function (a, b, method = "artifacts-only")
{
  stopifnot(is_commit(a), is_commit(b))

  if (identical(method, "artifacts-only")) {
    # name and its assigned identifier must match
    an <- sort(not_null(names(a$object_ids), character()))
    bn <- sort(not_null(names(b$object_ids), character()))
    return(identical(a$object_ids[an], b$object_ids[bn]))
  }

  stop("unknown comparison method: ", method)
}



#' Write commit to an object store.
#'
#' An assumption is made that all objects are already stored and only
#' the commit itself needs to be written to the object store.
#'
#' @param store An object store, e.g. [storage::filesystem].
#' @param commit A [commit] object.
#'
write_commit <- function (store, commit)
{
  stopifnot(is_commit(commit))
  stopifnot(storage::is_object_store(store))

  if (is.na(commit$id)) {
    commit$id <- storage::compute_id(commit)
  }

  # this should never happen because hash is computed from both objects
  # and parent id; if it does happen, something is SERIOUSLY broken
  if (storage::os_exists(store, commit$id)) {
    stop("commit already exists, aborting")
  }

  # TODO: update objects' tags: set parent commit id

  # store list of object pointers + basic 'history' tags
  id <- storage::os_write(store, list(objects = commit$object_ids, expr = commit$expr),
                          tags = list(class = class(commit), parent = commit$parent),
                          id = commit$id)

  invisible(commit)
}


# TODO rename to read_commit
commit_restore <- function (id, store, .data = TRUE)
{
  stopifnot(is_nonempty_character(id), storage::is_object_store(store))

  raw <- storage::os_read(store, id)
  co <- commit(raw$object$objects, raw$object$expr, raw$tags$parent, id)

  if (!isTRUE(.data)) return(co)
  commit_restore_data(co, store)
}


commit_restore_data <- function (co, store)
{
  co$objects <- lapply(co$object_ids, function (id) storage::os_read_object(store, id))
  co$tags <- lapply(co$object_ids, function(id) storage::os_read_tags(store, id))
  co
}


# TODO commit should have its own timestamp
commit_timestamp <- function (co, store)
{
  stopifnot(is_commit(co))
  time <- min(vapply(co$object_ids, function (id) {
    tags <- storage::os_read_tags(store, id)
    as.integer(tags$time)
  }, integer(1)))
  as.POSIXct(time, tz = 'UTC', origin = '1970-01-01')
}



#' @rdname commit
#' @export
#'
#' @param simple Show simplified printout.
#' @param header If extended output (`simple` is `FALSE`), print a header line.
#' @param ... Additional parameters to control printout.
#' @param store Optionally, the [storage::object_store] that holds the
#'        commit; defaults in the internal store of the R session.
#'
print.commit <- function (x, simple = FALSE, header = TRUE, ..., store)
{
  if (missing(store)) store <- internal_state$stash
  if (isTRUE(simple))
  {
    cat(crayon::green(storage::shorten(x$id)), ' : ',
        toString(x, simple = simple, store = store), '\n')
  }
  else
  {
    tag_print <- function (x) {
      if (!length(x)) return('')
      if (length(x) < 2) return(as.character(x))
      paste0('[', paste(x, collapse = ', '), ']')
    }

    # header
    if (isTRUE(header))
      cat('Commit : ', crayon::green(ifelse(is.na(x$id), '<no id>', x$id)), '\n')

    # contents
    obj <- x$objects
    iob  <- names(obj) != '.plot'

    print_tags <- function (tags) {
      tags <- vapply(tags, tag_print, character(1))
      cat(paste(names(tags), '=', tags, collapse = ', '))
    }

    cat('objects :\n')
    mapply(function (name, id) {
        cat('  ', crayon::yellow(name), ': ')
        print_tags(storage::os_read_tags(store, id))
        cat('\n')
      },
      name = names(x$objects)[iob],
      id = as.character(x$object_ids)[iob])

    if (!all(iob)) {
      id <- x$object_ids[[which(!iob)]]
      tags <- storage::os_read_tags(store, id)
      tags$class <- NULL
      cat('plot :\n  ')
      print_tags(tags)
    }
  }
}


toString.commit <- function (x, simple = FALSE, ..., store)
{
  if (missing(store)) store <- internal_state$stash

  paste(paste(names(x$objects), collapse = ', '), 'created on', commit_timestamp(x, store))
}


#' @export
# `.DollarNames.commit` <- function (x, pattern = "")
# {
#   grep(pattern, c("restore", names(x)), value = TRUE)
# }


#' @export
# `$.commit` <- function (x, i = "")
# {
#   if (isTRUE(i %in% names(x))) {
#     return(x[[i]])
#   }
#
#   # RStudio calls the operator so we cannot restore directly, but only
#   # after the user confirms the command and its return value is printed
#   if (identical(i, "restore")) {
#     return(structure(list(commit = x), class = 'restorer'))
#   }
#
#   stop("unknown option: ", i, call. = FALSE)
# }


#' @export
print.restorer <- function (x, ...)
{
  co <- commit_restore_data(x$commit, internal_state$stash)
  restore_commit(co)
  message(crayon::green('Commit restored'))
  print(co)
}


#' @import storage
to_long <- function (x, store = internal_state$stash)
{
  stopifnot(is.character(x), length(x) > 0)
  storage::enlongate(x, store)
}
