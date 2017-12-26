# Track user's interactions with R session.

#' Global state of the tracker.
#'
#' \describe{
#'   \item{tracking}{whether we are in the tracking state}
#'   \item{old_prompt}{prompt as set when loading the package}
#'   \item{stash}{local, file-system-based, object cache}
#' }
#'
internal_state <- new.env()



initiate_state <- function ()
{
  internal_state$tracking         <- FALSE
  internal_state$stash            <- create_stash()
  internal_state$task_callback_id <- NA
  internal_state$old_prompt       <- getOption("prompt")
  internal_state$last_commit      <- commit(list(), bquote(), NA_character_)
#  internal_state$last_commit_id   <- store_commit(emptyenv(), NA_character_, bquote(), state$stash)
}


create_stash <- function ()
{
  storage::filesystem(file.path(tempdir(), 'experiment-stash'),
                      create = TRUE)
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
  if (!isTRUE(internal_state$tracking) || !isTRUE(successful))
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



#' Toggle tracking mode.
#'
#' @export
#'
tracking_on <- function () {
  internal_state$tracking <- TRUE
  options(prompt = "[tracked] > ")
}


#' @name tracking_on
#' @export
tracking_off <- function () {
  internal_state$tracking <- FALSE
  options(prompt = internal_state$old_prompt)
}
