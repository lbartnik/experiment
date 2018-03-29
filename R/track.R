# Track user's interactions with R session.

#' Global state of the tracker.
#'
#' \describe{
#'   \item{tracking}{whether we are in the tracking state}
#'   \item{old_prompt}{prompt as set when loading the package}
#'   \item{stash}{local, file-system-based, object cache}
#' }
#'
#' @rdname internal_state
internal_state <- new.env()


#' @description `initiate_state` assigns the default values to all
#' parameters of the global `internal_state` object. By default it:
#' * creates an anonymous [storage::object_store] which is removed
#'   when R session exits
#' * does not turn tracking on (see [tracking_on])
#' * creates a "fake" parent commit which can become the root of a
#'   new history graph
#'
#' @rdname internal_state
#'
initiate_state <- function ()
{
  internal_state$stash            <- create_stash()
  internal_state$task_callback_id <- NA
  internal_state$old_prompt       <- getOption("prompt")
  internal_state$last_commit      <- commit(list(), bquote(), NA_character_)
}



#' A callback run after each top-level expression is evaluated.
#'
#' From [addTaskCallback()]: if the data argument was specified in
#' the call to addTaskCallback, that value is given as the fifth
#' argument.
#'
#' @param expr Expression for the top-level task.
#' @param result Result of the top-level task.
#' @param successful A logical value indicating whether it was
#'        successfully completed or not (always `TRUE` at present).
#' @param printed A logical value indicating whether the result was
#'        printed or not.
#'
#' @return A logical value indicating whether to keep this function in
#'         the list of active callbacks.
#'
#' @import grDevices
#'
task_callback <- function (expr, result, successful, printed)
{
  if (!isTRUE(successful))
    return(TRUE)

  tryCatch(
    error = function(e) warning('could not create a commit: ',
                                e$message, call. = FALSE),
    {
      last_plot <- tryCatch(recordPlot(), error = function(e)'error')
      if (identical(last_plot, 'error')) last_plot <- NULL

      # it's length of ls() because we don't care for hidden objects
      if (length(ls(globalenv())))
        update_current_commit(internal_state, globalenv(), last_plot, expr)
    }
  )

  TRUE
}


#' @description `update_current_commit` is a part of `task_callback`
#' made separate due to testing purposes.
#'
#' @param state Global state, an [environment()]; passed as a parameter
#'        for testing purposes.
#' @param env Environment this commits represents.
#' @param plot The last plot (see [recordPlot()]).
#'
#' @name task_callback
#' @export
#'
update_current_commit <- function (state, env, plot, expr)
{
  objects <- store_environment(state$stash, env, expr)

  # if the current plot looks the same as the last one, do not update at all
  .plot <- plot_as_svg(plot)
  if (!is.null(.plot) && !svg_equal(.plot, state$last_commit$objects$.plot)) {
    objects$.plot <- store_plot(state$stash, .plot, env, expr, objects)
  }

  # now create and process the commit
  co <- commit(objects, expr, state$last_commit$id)

  # if there are new artifacts, store a new commit
  if (!commit_equal(co, state$last_commit, "artifacts-only")) {
    state$last_commit <- write_commit(state$stash, co)
  }

  invisible(state$last_commit$id)
}



#' @rdname store_environment
store_environment <- function (store, env, expr)
{
  stopifnot(is.environment(env))
  stopifnot(storage::is_object_store(store))

  ids <- lapply(as.list(env), function (obj) {
    obj  <- strip_object(obj)
    id <- storage::compute_id(obj)
    if (storage::os_exists(store, id)) return(id)
    storage::os_write(store, obj, id = id, tags = auto_tags(obj))
  })

  # assign parents
  lapply(ids, function (id) {
    tags <- storage::os_read_tags(store, id)
    if ('parents' %in% names(tags)) return()

    # TODO this can get confused if the expression changes multiple objects
    parents <- extract_parents(env, expr)
    tags$parents <- names_to_ids(parents, ids)

    storage::os_update_tags(store, id, tags)
  })

  ids
}


#' @rdname store_environment
store_plot <- function (store, plot, env, expr, ids)
{
  id <- storage::compute_id(plot)
  if (storage::os_exists(store, id)) return(id)

  tags <- auto_tags(plot)
  # TODO this can get confused if the expression changes multiple objects
  parents <- extract_parents(env, expr)
  tags$parents <- names_to_ids(parents, ids)

  storage::os_write(store, plot, id = id, tags = tags)
}


#' @rdname store_environment
extract_parents <- function (env, expr)
{
  stopifnot(is.environment(env))

  # add the "parent objects" tag using "defer"
  fn <- function(){}; body(fn) <- expr

  df <- defer::defer_(fn, .caller_env = env, .extract = TRUE)
  ev <- defer::extract_variables(df)
  ef <- defer::extract_functions(df)

  c(names(ev), setdiff(names(ef), 'entry'))
}


#' @description `names_to_ids` is a simple helper function that maps
#' names to identifiers and makes sure all requested names are mapped.
#' Important details is to keep the vector of parents named: only this
#' way an object/plot can be re-computed; without names there is no way
#' to match object ids into the associated expression.
#'
#' @rdname store_environment
names_to_ids <- function (what, mapping)
{
  stopifnot(all(what %in% names(mapping)))
  vapply(what, function (n) mapping[[n]], character(1))
}


#' Removes references to environments.
#'
#' Some objects (e.g. formula, lm) store references to environments
#' in which they were created. This function replaces each such reference
#' with a reference to `emptyenv()`.
#'
#' As much as possible, this function tries not to make any copies of
#' the original data. This is because the address of the object might
#' be used to determine whether object's identifier needs to be computed
#' which might be a costly operation.
#'
#' @param obj Object to be processed.
#' @return `obj` with environment references replaced by `emptyenv()`
#'
#' @rdname store_environment
#'
strip_object <- function (obj)
{
  if (is.symbol(obj)) return(obj)

  # TODO should we disregard any environment?
  if (is.environment(obj)) return(emptyenv())

  attrs <- if (!is.null(attributes(obj))) lapply(attributes(obj), strip_object)

  if (is.list(obj)) {
    obj_tmp <- lapply(obj, strip_object)
    # use stripped object only if stripping actually changed something
    obj_lst <- lapply(obj, function(x)x)
    if (!identical(obj_tmp, obj_lst)) {
      obj <- obj_tmp
    }
  }
  if (!identical(attributes(obj), attrs)) {
    attributes(obj) <- attrs
  }

  obj
}



# --- tracker ----------------------------------------------------------

#' Work with commands recorded in the current R session.
#'
#' @description `tracker_replay` repeats a sequence of commands.
#'
#' @rdname tracker
#'
#' @export
#' @examples
#' \dontrun{
#' # initial sequence
#' x <- 1
#' y <- x + 2
#' z <- y ** 2
#' w <- sqrt(y)
#'
#' # alterations: explicit value
#' tracker_replay(x = 2)
#' # alterations: replace the same object
#' x <- 2
#' tracker_replay(x)
#' # alterations: name substitution
#' v <- 3
#' tracker_replay(x = v)
#' # alterations: only some objects; w is not replayed
#' tracker_replay(output(z), replace(x = 4))
#'
#' # show all branches created in those replays
#' tracker$branch
#' }
#'
tracker_replay <- function (..., store, last_id)
{
  # A new branch is created that is accessible by browser (GUI/text) but
  # does not replace the current session.

  # 1. extract details from ...
  #    - handle output and replace TODO?
  #    - make sure ... are named or point to a symbol
  # 2. create substitution aggregate
  # 3. consult aggregate with commits in a straight line from last to root
  #    - verify which objects are being substituted and which serves as
  #      substitutes
  #    - compare names
  #    - compare expressions; what is the measure of similarity? TODO?
  # 4. objects which serve as substitutes can be tracked to their commits of
  #    origin by their ids; the earliest commit where a substitute is defined
  #    ends the replay pipeline
  # 5. identify commits where the originals appear; we will start replaying
  #    with the earliest original and stop just before the first substitution
  # 6. place commits that inject substitution just after each commit where
  #    an original appears/is created
  # 7. if there is an output filter defined, apply it to the sequence of
  #    commits; most probably only a few will be filtered out as we replay
  #    both the commit where the desired object is created and the whole path
  #    of objects that lead to it
  # 8. stepping from the first substituted, replay commands:
  #    - if commit is either an origin of replayed output or on a path to one,
  #      re-evaluate
  #    - if command creates an object that is supposed to be substituted, it
  #      should result in the same object being created; otherwise this might
  #      be a user error - specifying a substitute for an object that depends
  #      on an earlier substitution; fail or show a warning
  #    - finally, commands will create one of the expected products; re-evaluate
  #      and store its output

  if (missing(store)) store <- internal_state$stash

  # TODO this is a stop-gap; obtain the real values passed via ...
  # determine what gets re-evaluated and what is injected into the replay
  output <- character() # ALL
  replace <- list(objects = list(x = 100L), originals = list()) # names point to originals
  replace$pointers <- lapply(replace$objects, function(o) list(id = storage::compute_id(o)))

  # obtain the path from the current R session to root; it we be repeatedly
  # examined during the 'replay' procedure
  full <- graph(store, TRUE)
  path <- graph_subset(full, 'path', last_id, 'root')

  replace_ids <- vapply(replace$pointers, `[[`, i = 'id', character(1))
  browser()

  # 1. examine the path: match output objects to their commits of origin
  for (commit in path) {
    intro_nms <- introduced_in(full, commit$id)
    intro_ids <- as.character(commit$object_ids[intro_nms])

    # objects introduced in this commit: id match
    ii <- grep(intro_ids, replace_ids, fixed = TRUE)
    for (name in intro_nms[ii]) {
      # only if the commit hasn't been found yet; use the most recent
      # commit of replacement
      if (is.null(replace$pointers[[name]]$commit_id)) {
        replace$pointers[[name]]$commit_id <- commit$id
      }
    }
  }


  # 2. match the replace object with their originals; look at the commit
  #    path in the reverse order
  for (commit in rev(path)) {
    nm <- intersect(names(replace$objects) %in% introduced_in(full, commit$id))
    lapply(nm, function (n) {
      replace$originals[[n]] <- list(commit_id = commit$id, id = commit$object_ids[[n]])
    })
  }

  replace
}


#' @description `tracker_session` works with the current R session and
#' shows its summary; It returns a `"tracked"` object which can be printed
#' (console) and plotted (Shiny widget).
#'
#' @rdname tracker
#'
#' @export
#'
tracker_session <- function ()
{
  tracker_session_(internal_state$stash)
}


#' @description `tracker_session_` is a non-interactive alternative
#' which can work with historical R sessions.
#'
#' @rdname tracker
#'
#' @export
#'
tracker_session_ <- function (store)
{
  with_class(graph(store, TRUE), "tracked_session")
}


#' @description `tracker_sequence` shows a sequence of commands leading
#' from the most current to the very beginning of the current R session.
#' It can be printed (console) and plotted (Shiny widget).
#'
#' @rdname tracker
#'
#' @export
#'
tracker_sequence <- function ()
{
  tracker_sequence_(internal_state$stash, internal_state$last_commit$id)
}


#' @description `tracker_sequence_` is a non-interactive alternative
#' which can work with historical R sessions.
#'
#' @rdname tracker
#'
#' @export
#'
tracker_sequence_ <- function (store, last_id)
{
  with_class(graph_subset(graph(store, TRUE), 'path', last_id, 'root'),
             "tracked_sequence")
}



# --- tracked sequence -------------------------------------------------

# TODO rename tracked_sequence to recording or recorded_sequence or recorded

#' @rdname tracked_sequence
#' @name tracked_sequence
#'
#' @title Sequence of R commands.
#'
#' @description A `tracked_sequence` object represents a part of the
#' history of R session as recorded by the [tracker]. It can be printed
#' to the console, plotted via a `Shiny` widget but, most importantly,
#' it can be *replayed* in a variety of ways, including arbitrary
#' substitutions of objects created or stored in any of the past states
#' of R session.
#'
#' @examples
#' \dontrun{
#' x <- 1
#' y <- x + 1
#' v <- y + 100
#' z <- (x + 2) * z
#'
#' s <- tracker_sequence()
#' print(s)
#' summary(s)
#' plot(s)
#'
#' # replay the whole sequence replacing x in the initial commit (a) with 100
#' t <- s(a:x = 100)
#' print(t)
#'
#' # show the object x in commit a
#' t$a$x
#'
#' # export all data as a tibble
#' as_tibble(t)
#' }


is_tracked_sequence <- function(x) inherits(x, 'tracked_sequence')


#' @rdname tracked_sequence
#' @export
print.tracked_sequence <- function (x, ...)
{
  stopifnot(is_graph(x))

  ccat_(list(default = '<tracked sequence>:', green = 'main branch', default = '\n\n'))

  # TODO use for both tree and sequence; print info about branches;
  #      print the full sequence of the main branch (the one which
  #      belongs to the current R session)

  # TODO sequence can be longer than letters
  mapply(commit = rev(x), letter = letters[seq_along(x)],
         function (commit, letter)
  {
    new <- introduced_in(x, commit$id)
    old <- setdiff(names(commit$object_ids), new)

    ccat0('silver', letter, ': ')
    ccat('green', new)
    if (length(new) && length(old)) cat(' ')
    cat(old, '\n')

    ccat('white', strwrap(format_expression(commit$expr), indent = 3))
    cat('\n')
  })

  ccat('silver', '\nSee ?tracked_sequence for more info.')

  invisible(x)
}


#' @rdname tracked_sequence
#' @export
summary.tracked_sequence <- function (x, ...)
{
  # TODO this should return a summary object and the current body should
  #      go into a print for that summary, e.g. print.recorded_summary
  stopifnot(is_graph(x))

  cat('<tracked sequence>:')
  ccat('green', 'possible substitutions\n\n')
  ccat_(list(silver = '(', white = 'commit:', default = 'name', silver = ' <original value>):\n'))

  delim <- ':'
  subs <- mapply(commit = x, letter = letters[seq_along(x)],
         function (commit, letter)
  {
    mapply(name = names(commit$objects), object = commit$objects,
           function (name, object)
    {
      ccat0('white', '   ', letter, delim)
      cat(name, '  ')
      printout <- format(object)
      if (length(printout) > 1 || nchar(printout) > 20) printout <- pillar::obj_sum(object)
      ccat0('silver', printout, '\n')
    })

    paste0(letter, '.', names(commit$objects))
  })

  subs <- unlist(subs)
  subs <- unique(c(first(subs), last(subs)))
  subs <- paste(subs, '=', sample(1e5, length(subs)), collapse = ', ')

  ccat0('silver', '\nExample: tracker_execute(x, ', subs, ')\n')

  invisible(x)
}


#' @rdname tracked_sequence
#' @export
plot.tracked_sequence <- function (x, ...)
{
  stopifnot(is_graph(x))
  render_steps(graph_to_steps(x))
  invisible(x)
}


#' @rdname tracked_sequence
#' @export
as_tibble.tracked_sequence <- function (x, ...)
{
  stopifnot(is_graph(x))

  objects <- lapply(x, `[[`, i = 'objects')
  objects <- lapply(objects, function (o) {
    tibble(name = names(o), object = unname(o))
  })

  tibble::tibble(commit_id = names(x), objects)
}


#' @rdname tracked_sequence
#' @export
.DollarNames.tracked_sequence <- function (x, pattern = "")
{
  stopifnot(is_graph(x))
  grep(pattern, names(x), value = TRUE)
}


#' @rdname tracked_sequence
#' @export
`$.tracked_sequence` <- function (x, i)
{
  stopifnot(is_graph(x))
  stopifnot(i %in% names(x))
  x[[i]]
}


#' @export
tracker_execute <- function (x, ...)
{
  stopifnot(is_tracked_sequence(x))
  rlang::enquos(...)
}


# ----------------------------------------------------------------------


#' Restore a snapshot from history.
#'
#' Restores a historical [commit] from commit or object `id`:
#' * commit `id` brings back that specific commit
#' * object `id` brings back the earlies commit where that
#'   object can be found, which should be the commit where that
#'   object has been created
#'
#' @param id `commit` or object identifier, a SHA1 string (__long id__)
#'        or its first 8 characters (__short id__).
#'
#' @export
#' @import storage
#'
restore <- function (id)
{
  long_id <- enlongate(id, internal_state$stash)
  if (!os_exists(internal_state$stash, long_id)) {
    stop('cannot find commit or object with id ', id, call. = FALSE)
  }

  tags <- os_read_tags(internal_state$stash, long_id)

  # if id does not point to a commit, it might be an object from a commit
  if (!identical(tags$class, 'commit')) {
    g <- graph(internal_state$stash, .data = FALSE)
    co <- find_first_parent(g, id)
    if (is.null(co)) {
      stop('cannot find commit for object ', id, call. = FALSE)
    }

    # if found, pass it on to the final line
    long_id <- co$id
  }

  # restore the actual commit
  restore_commit(internal_state, long_id, globalenv())
}


#' @import storage
restore_commit <- function (state, id, env)
{
  stopifnot(is_object_store(state$stash))
  stopifnot(os_exists(state$stash, id))

  co <- commit_restore(id, state$stash, .data = TRUE)

  state$last_commit <- co
  rm(list = ls(envir = env), envir = env)

  mapply(function (name, value) assign(x = name, value = value, envir = env),
         name = names(co$objects), value = co$objects)

  # clean the current plot and restore the one that came with the commit
  try(dev.off(), silent = TRUE)

  # TODO currently there seems to be no way to plot *from* SVG onto
  #      the interactive graphic device
  #if (!is.null(co$objects$.plot)) {
  #  replayPlot(co$objects$.plot)
  #}

  invisible()
}



#' @rdname tracking
#' @title Turn tracking on or off
#'
#' @description `tracking_on` turns the tracking mode on, which is
#' signaled by a new prompt, `[tracked] > `. It also attaches to an
#' object store (see [storage::object_store]), if one can be found under
#' `path`. If no object store can be found, and `path` points to a
#' non-existing directory whose parent directory does exist, then that
#' top-level directory is created and a new object store is created in
#' it.
#'
#' @details When an existing object store is found, and it is not empty,
#' that is, it contains artifacts and [commit]s from previous R sessions,
#' the current R session is set as a continuation of one of those
#' `commit`s. However, if the current *global environment* (see
#' [globalenv]) is not empty, it needs to be replaced or merged with the
#' chosen `commit`. To that extent, the `.global` argument is consulted.
#' It can take one of the following values:
#' * `"abort"` - the default, aborts the tracking of `globalenv` is not
#'   empty
#' * `"replace"` - replace the contents of `globalenv` with the chosen
#'   commit
#' * `"merge"` - merge the contents of `globalenv` with the chosen commit;
#'   this creates a new commit in the process, which is immediately written
#'   back to the object store
#'
#' When tracking is enabled a task callback installed via
#' [addTaskCallback]. It is used to examine the contents of the
#' *global environment*  each time an R command is successfully executed.
#'
#'
#' @param path Where to locate the object store (see [storage::object_store]).
#' @param .global How to handle [globalenv] when it is not empty.
#'
#' @export
#' @examples
#' \dontrun{
#' # if no object store exists, a new one is created under the
#' # default "project-store" directory located in the current
#' # working directory
#' tracking_on()
#'
#' # as above, but the new directory is "my-store"
#' tracking_on("my-store")
#' }
#'
tracking_on <- function (path = file.path(getwd(), "project-store"), .global = "abort")
{
  # first check if there is already an object store under the given path,
  # and either choose the existing one or prepare a temporary stash
  store <- prepare_object_store(path)

  reattach_to_store(internal_state, store, globalenv(), .global)

  # make sure the callback is removed
  if (!is.na(internal_state$task_callback_id)) {
    removeTaskCallback(internal_state$task_callback_id)
  }

  internal_state$task_callback_id <- addTaskCallback(task_callback)
  options(prompt = "[tracked] > ")
}


#' @rdname tracking
#' @description `tracking_off` reverses the effect of `tracking_on`. It
#' removes the callback and brings back the original value of that R
#' session's prompt.
#'
#' @export
#'
tracking_off <- function () {
  if (!is.na(internal_state$task_callback_id)) {
    removeTaskCallback(internal_state$task_callback_id)
    internal_state$task_callback_id <- NA
  }

  if (!is.na(internal_state$old_prompt)) {
    options(prompt = internal_state$old_prompt)
    internal_state$old_prompt <- NA_character_
  }

  internal_state$stash <- create_stash()
}


# --- object store -----------------------------------------------------

#' Manage object stores.
#'
#' @name internal_object_store
#' @rdname internal_object_store
NULL


#' @rdname internal_object_store
#'
#' @description `discover_object_store` starts with with given `path`
#' and searches for a configuration file (*not implemented yet*) or an
#' existing object store that `experiment` can use for the current R
#' session. If a store can be found, an [storage::object_store] object
#' is returned; otherwise, a `NULL` value is returned.
#'
#' @param path Path to be examined.
#' @return `discover_object_store` returns a `character` vector whose
#' elemnets are paths to existing object stores.
#'
discover_object_store <- function (path = getwd())
{
  stopifnot(dir.exists(path))

  # TODO support for configuration files

  if (storage::is_filesystem_dir(path, empty_ok = TRUE)) return(path)

  dirs <- Filter(function (x) isTRUE(file.info(x)$isdir),
                 list.files(path, include.dirs = TRUE, full.names = TRUE, recursive = FALSE))
  isos <- vapply(dirs, storage::is_filesystem_dir, logical(1))

  dirs[isos]
}


#' @rdname internal_object_store
#'
#' @description `prepare_object_store` opens an existing object store
#' (via `discover_object_store`) or creates a new, temporary one.
#'
#' @param .silent Reduce the verbosity to warnings and errors.
#'
#' @return `prepare_object_store` returns an [storage::object_store] object.
#'
prepare_object_store <- function (path, .silent = !interactive())
{
  # if the condition is true, user requested to create a store
  if (!dir.exists(path) && dir.exists(dirname(path))) {
    warning('creating a store named "', basename(path), '" under "', dirname(path), '"',
            call. = FALSE)
    return(storage::filesystem(path, create = TRUE))
  }

  # if the path exists, look for an object store under it
  x <- discover_object_store(path)
  if (length(x) == 1) {
    if (isFALSE(.silent)) message('using an existing object store: "', x, '"')
    return(storage::filesystem(x, create = FALSE))
  }

  # if none or more than one found
  if (length(x) > 1) {
    stop('found more than one object store under "', path, '": "',
         paste(x, collapse = '", "'), '", choose the one you wish to use',
         call. = FALSE)
  }

  temp_path <- file.path(tempdir(), 'experiment-stash')
  warning(' no object stores found, creating a temporary one under "',
          temp_path, '"; objects will be lost when R session exits',
          call. = FALSE)

  create_stash(temp_path)
}


#' @rdname internal_object_store
#'
#' @description `create_stash` creates an empty object store under the
#' given `path`.
#'
create_stash <- function (path = file.path(tempdir(), 'experiment-stash'))
{
  # force creation in case the path does not exist yet
  st <- storage::filesystem(path, create = TRUE)
  stopifnot(storage::is_filesystem(st))
  st
}


choose_store <- function (path, .create = FALSE)
{

}


#' @rdname internal_object_store
#'
#' @description `reattach_to_store` makes `store` the current object
#' store and determines which `commit` present in that store should
#' become the current *parent commit* (in *git* known as `HEAD`). It
#' is used only in [tracking_on] but it is separate from it for
#' testing purposes.
#'
#' @param state The [internal_state] object or a testing mock object.
#' @param store A [storage::object_store] object.
#' @param env The [globalenv] or testing mock environment.
#' @param .global Action to take when reading the [commit] into `env`.
#'
reattach_to_store <- function (state, store, env, .global = "abort", .silent = !interactive())
{
  # TODO add "ask" in interactive mode
  stopifnot(.global %in% c("abort", "overwrite", "merge"))

  # check whether there is a historical commit to continue from; if not,
  # attach are immediately return
  g <- graph(store)
  if (!length(g)) {
    if (isFALSE(.silent)) message("Attached to an empty store.")
    state$stash <- store
    return(invisible())
  }

  # if there is something in the store
  lv <- graph_leaves(g)
  if (!length(lv)) {
    return(invisible())
  }

  if (length(lv) > 1) {
    if (isTRUE(.silent)) {
      stop("more than one commit could be restored but running in ",
           "silent mode; aborting", call. = FALSE)
    }

    choices <- lapply(lv, toString, simple = TRUE, store = store)
    names(choices) <- storage::shorten(names(choices))

    ans <- showChoiceDialog(
      "Restore R session",
      paste("There are", length(lv), "branches in the history tree. Choose the one",
            "to be restored."),
      choices
    )

    if (is.null(ans)) {
      stop("No choice has been made, aborting", call. = FALSE)
    }

    ct <- nth(lv, match(ans, names(choices)))
  }
  else {
    ct <- first(lv)
  }

  # if there is nothing in the current R session, simply reattach
  # in the chosen point in history
  if (!length(ls(env, all.names = FALSE))) {
    state$stash <- store
    restore_commit(state, ct$id, env)

    if (isFALSE(.silent)) {
      cat('Attached to a new object store. R session reset to commit "', ct$id, '"\n')
      print(ct, store = store)
    }

    return(invisible())
  }

  # if there is something in the current R session (env == globalenv()),
  # see what to do: abort, overwrite, merge?
  if (identical(.global, "abort")) {
    stop("global environment is not empty, cannot restore commit, aborting",
         call. = FALSE)
  }

  # overwrite - clean globalenv and load commit instead
  if (identical(.global, "overwrite")) {
    message <- 'global environment is not empty, "overwrite" chosen, replacing globalenv with the historical commit'

    if (isTRUE(.silent)) warning(message, call. = FALSE)
    else {
      cat(crayon::red(message), '\n\n')
      print(ct, store = store)
    }

    rm(list = ls(envir = env, all.names = TRUE), envir = env)
    state$stash <- store
    restore_commit(state, ct$id, env)
  }

  # merge the commit with the current globalenv; create a new commit
  # and write it back to the store
  if (identical(.global, "merge")) {
    message <- 'global environment is not empty, "merge" chosen, merging globalenv with the historical commit'

    ct <- commit_restore_data(ct, store)
    merged_contents <- as.environment(c(ct$objects, as.list(env, all.names = TRUE)))

    state$last_commit <- ct
    state$stash <- store
    ct <- update_current_commit(state, merged_contents, NULL, bquote())

    if (isTRUE(.silent)) warning(message, call. = FALSE)
    else {
      cat(crayon::red(message))
      print(ct, store = store)
    }

    rm(list = ls(envir = env, all.names = TRUE), envir = env)
    mapply(function (name, value) assign(name, value, envir = env),
           name = names(merged_contents), value = as.list(merged_contents))
  }

  return(invisible())
}
