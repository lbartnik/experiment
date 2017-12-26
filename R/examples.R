# Suppress checks in `simulate_london_meters`.
utils::globalVariables(c('LCLid', 'tstp', 'energy_kWh', 'meter', 'timestamp', 'usage', 'dow', 'hour'))

#' Simulations and examples.
#'
#' These functions populate sessions' history cache with a complete
#' history of data exploration.
#'
#' @description `simulate_london_meters` loads and examines a subset
#' of __London meters__ data; see [energy] and the introductory vignette.
#'
#' @param overwrite If current stash contains objects, setting `overwrite`
#'        to `TRUE` will remove them prior to running simulation.
#'
#' @export
#'
simulate_london_meters <- function (overwrite = TRUE)
{
  requireNamespace('dplyr', quietly = TRUE)
  requireNamespace('lubridate', quietly = TRUE)
  requireNamespace('magrittr', quietly = TRUE)
  requireNamespace('ggplot2', quietly = TRUE)
  requireNamespace('stats', quietly = TRUE)
  requireNamespace('readr', quietly = TRUE)

  `%<>%` <- magrittr::`%<>%`
  `%>%` <- magrittr::`%>%`

  clean_stash(overwrite, internal_state)
  user_space <- eval_space()

  user_space$simulate(
    input <-
      system.file("extdata/block_62.csv", package = "experiment") %>%
      readr::read_csv(na = 'Null') %>%
      dplyr::rename(meter = LCLid, timestamp = tstp, usage = `energy_kWh`) %>%
      dplyr::filter(meter %in% c("MAC004929", "MAC000010", "MAC004391"),
                    lubridate::year(timestamp) == 2013)
  )

  # remember the commit id so that later we can come back to this point in history
  go_back <- user_space$simulate(
    input %<>%
      dplyr::mutate(timestamp = lubridate::floor_date(timestamp, 'hours')) %>%
      dplyr::group_by(meter, timestamp) %>%
      dplyr::summarise(usage = sum(usage))
  )

  # dplyr adds attributes to objects when filter is called
  # it's probably some kind of smart pre-computed cache but
  # it messes up object tracking
  #
  # if filter is not a separate step, use subset() instead of
  # filter() to maintain the same object id between commits
  user_space$simulate(
    input %<>% dplyr::filter(meter == "MAC004929")
  )

  user_space$simulate(
    with(input, plot(timestamp, usage, type = 'p', pch = '.'))
  )

  user_space$simulate(
    x <-
      input %>%
      dplyr::mutate(hour = lubridate::hour(timestamp),
                    dow  = lubridate::wday(timestamp, label = TRUE)) %>%
      dplyr::mutate_at(dplyr::vars(hour, dow), dplyr::funs(as.factor)) %>%
      dplyr::group_by(hour, dow) %>%
      dplyr::summarise(usage = mean(usage, na.rm = TRUE))
  )

  user_space$simulate(
    with(x, plot(hour, usage))
  )

  user_space$simulate(
    ggplot2::ggplot(x) +
      ggplot2::geom_point(ggplot2::aes(x = hour, y = usage)) +
      ggplot2::facet_wrap(~dow)
  )

  user_space$simulate(
    x <-
      input %>%
      dplyr::mutate(hour = lubridate::hour(timestamp),
                    dow  = lubridate::wday(timestamp)) %>%
      dplyr::mutate_at(dplyr::vars(hour, dow), dplyr::funs(as.factor))
  )

  user_space$simulate(
    ggplot2::ggplot(x) +
      ggplot2::geom_boxplot(ggplot2::aes(x = hour, y = usage)) +
      ggplot2::facet_wrap(~dow)
  )

  user_space$simulate(
    m <- stats::lm(usage ~ hour:dow, x)
  )

  message('Restoring commit ', go_back)
  restore_commit(internal_state, go_back, user_space$session)

  # now try a different house
  user_space$simulate(
    input %<>% dplyr::filter(meter == "MAC000010")
  )

  user_space$simulate(
    x <-
      input %>%
      dplyr::mutate(hour = lubridate::hour(timestamp),
                    dow  = lubridate::wday(timestamp)) %>%
      dplyr::mutate_at(dplyr::vars(hour, dow), dplyr::funs(as.factor))
  )

  user_space$simulate(
    ggplot2::ggplot(x) +
      ggplot2::geom_boxplot(ggplot2::aes(x = hour, y = usage)) +
      ggplot2::facet_wrap(~dow)
  )

  # go back again, and try the third house
  message('Restoring commit ', go_back)
  restore_commit(internal_state, go_back, user_space$session)

  user_space$simulate(
    input %<>% dplyr::filter(meter == "MAC004391")
  )

  user_space$simulate(
    x <-
      input %>%
      dplyr::mutate(hour = lubridate::hour(timestamp),
                    dow  = lubridate::wday(timestamp)) %>%
      dplyr::mutate_at(dplyr::vars(hour, dow), dplyr::funs(as.factor))
  )

  user_space$simulate(
    ggplot2::ggplot(x) +
      ggplot2::geom_boxplot(ggplot2::aes(x = hour, y = usage)) +
      ggplot2::facet_wrap(~dow)
  )

  invisible(user_space)
}


# Suppress checks in `simulate_modelling`.
utils::globalVariables(c('iris'))

simulate_modelling <- function ()
{

  user_space <- eval_space()

  user_space$simulate(x <- stats::lm(Sepal.Width ~ Sepal.Length, iris))
  user_space$simulate(iris2 <- iris)
  user_space$simulate(iris2$Sepal.Length <- iris2$Sepal.Length ** 2)
  user_space$simulate(y <- stats::lm(Sepal.Width ~ Sepal.Length, iris2))
}



eval_space <- function ()
{
  try(dev.off(), silent = TRUE)

  e <- new.env()
  e$simulate <- simulate_user_command
  environment(e$simulate) <- e

  e$session <- new.env(parent = globalenv())

  structure(e, class = 'eval_space')
}


#' @importFrom utils capture.output
simulate_user_command <- function (expr)
{
  env <- parent.env(environment())
  stopifnot(inherits(env, 'eval_space'))

  expr <- substitute(expr)
  message("Evaluating: ", deparse(expr)[[1]], "...")

  # print is necessary for graphics, but we don't want to see the
  # output on the console, thus - print and capture at the same time
  eval_expr <- substitute(print(expr), list(expr = expr))
  capture.output(eval(eval_expr, env$session, enclos = baseenv()))

  plot <- tryCatch(recordPlot(), error = function(e)'error')
  if (identical(plot, 'error')) plot <- NULL

  update_current_commit(internal_state, env$session, plot, expr)
}


clean_stash <- function (overwrite, state)
{
  if (length(storage::os_list(state$stash))) {
    if (!isTRUE(overwrite)) {
      stop("stash is not empty and `overwrite` is FALSE, aborting",
           call. = FALSE)
    }

    warning("stash is not empty and `overwrite` is TRUE, removing data", call. = FALSE)
    storage::os_remove(state$stash)
    state$stash <- create_stash()
  }

  state$last_commit <- commit(list(), bquote(), NA_character_)
}


#' @import storage
modelling <- function (overwrite = FALSE, state)
{
  if (missing(state)) state <- internal_state
  clean_stash(overwrite, state)

  path <- file.path(system.file("examples", package = "experiment"), 'modelling')
  file.copy(from = list.files(path, full.names = TRUE),
            to = as.character(state$stash),
            recursive = TRUE)

  g <- graph(state$stash)
  l <- which.max(vapply(g, `[[`, numeric(1), i = 'level'))
  state$last_commit <- g[[l]]$id

  invisible()
}

