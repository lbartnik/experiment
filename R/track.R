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
#' @param expr Expression that created this environment.
#'
#' @name task_callback
#' @export
#'
update_current_commit <- function (state, env, plot, expr)
{
  # prepare the list of objects
  env <- as.list(env)
  env$.plot <- plot_as_svg(plot)

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

  path <- tempfile(fileext = ".svg")

  # TODO if `pl` has been recorded without dev.control("enable"), the
  #      plot might be empty; it might be necessary to check for that

  svg(path)
  replayPlot(pl)
  dev.off()

  contents <- readBin(path, "raw", n = file.size(path))
  jsonlite::base64_enc(contents)
}



restore_historical_commit <- function (co)
{
  stopifnot(is_commit(co))

  internal_state$last_commit <- co$id
  rm(list = ls(envir = globalenv()), envir = globalenv())
  mapply(function (name, value) assign(x = name, value = value, envir = globalenv()),
         name = names(co$objects), value = co$objects)
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
