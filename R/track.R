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
#' * does not turn tracking on (see [tracking_in])
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
  # prepare the list of objects
  env <- as.list(env)
  env$.plot <- plot_as_svg(plot)

  # if the current plot look sthe same as the last one, do not update at all
  if (svg_equal(env$.plot, state$last_commit$objects$.plot)) {
    env$.plot <- state$last_commit$objects$.plot
  }

  # TODO can both, env and plot, change as a result of a single command?

  # now create and process the commit
  co <- commit(env, expr)

  # if there is new data...
  if (!commit_equal(co, state$last_commit))
  {
    # ... and then write down actual data
    parent(co) <- state$last_commit$id
    state$last_commit <- commit_store(co, state$stash)
  }

  invisible(state$last_commit$id)
}


#' Returns a base64-encoded, SVG plot.
#'
#' @param pl Plot recorded by [recordPlot()].
#' @return `character` string, base64-encoded SVG plot.
#' @import jsonlite
#'
plot_as_svg <- function (pl)
{
  if (is.null(pl)) return(NULL)

  # TODO use svglite::stringSVG

  path <- tempfile(fileext = ".svg")

  # TODO if `pl` has been recorded without dev.control("enable"), the
  #      plot might be empty; it might be necessary to check for that

  svg(path)
  replayPlot(pl)
  dev.off()

  contents <- readBin(path, "raw", n = file.size(path))
  jsonlite::base64_enc(contents)
}


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

  reattach_to_store(state, store, globalenv(), .global)

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
#' @return `prepare_object_store` returns an [storage::object_store] object.
#'
prepare_object_store <- function (path, silent = !interactive())
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
    if (isFALSE(silent)) message('using an existing object store: "', x, '"')
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
reattach_to_store <- function (state, store, env, .global, .silent = !interactive())
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
    if (!interactive()) {
      stop("more than one commit could be restored but running in ",
           "non-interactive mode; aborting", call. = FALSE)
    }

    if (isFALSE(.silent)) {
      message("there are ", length(lv), " commit(s) that can become the ",
              "HEAD: ", paste(names(lv), collapse = ", "))
      lapply(lv, function (lf) print(lf, simple = TRUE, store = store))
    }

    repeat {
      ans <- readline("1-based index or id: ")
      if (!is.na(i <- match(ans, names(lv)))) ans <- i
      ans <- tryCatch(as.integer(ans), warning = function (e) NULL)
      if (!is.null(ans) && between(ans, 1, length(lv))) break
      message("could not recognize that choice...")
    }

    ct <- nth(lv, ans)
  }
  else {
    ct <- first(lv)
  }

  # if there is nothing in the current R session, simply reattach
  # in the chosen point in history
  if (!length(env)) {
    state$stash <- store
    restore_commit(state, ct$id, env)

    if (isFALSE(.silent)) {
      message('Attached to a new object store. R session reset to commit "', ct$id, '"')
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
    warning('global environment is not empty, "overwrite" chosen, replacing ',
            "globalenv with the historical commit", call. = FALSE)

    rm(list = ls(envir = env, all.names = TRUE), envir = env)
    state$stash <- store
    restore_commit(state, ct$id, env)
  }

  # merge the commit with the current globalenv; create a new commit
  # and write it back to the store
  if (identical(.global, "merge")) {
    warning('global environment is not empty, "merge" chosen, merging ',
            'globalenv with the historical commit', call. = FALSE)

    ct <- commit_restore_data(ct, store)
    merged_contents <- as.environment(c(ct$objects, as.list(env, all.names = TRUE)))

    state$last_commit <- ct
    state$stash <- store
    ct <- update_current_commit(state, merged_contents, NULL, bquote())

    if (isFALSE(.silent)) print(ct, store = store)

    rm(list = ls(envir = env, all.names = TRUE), envir = env)
    mapply(function (name, value) assign(name, value, envir = env),
           name = names(merged_contents), value = as.list(merged_contents))
  }

  return(invisible())
}
