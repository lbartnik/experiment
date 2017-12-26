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
#' @importFrom readr read_csv
#' @importFrom dplyr filter rename mutate group_by summarise mutate_at vars funs
#' @importFrom lubridate floor_date hour wday year
#' @importFrom ggplot2 ggplot facet_wrap geom_point geom_boxplot aes
#' @importFrom magrittr %<>% %>%
#' @importFrom stats lm
#'
#' @export
#'
simulate_london_meters <- function (overwrite = TRUE)
{
  clean_stash(overwrite, internal_state)
  user_space <- eval_space()

  user_space$simulate(
    input <-
      system.file("extdata/block_62.csv", package = "experiment") %>%
      readr::read_csv(na = 'Null') %>%
      rename(meter = LCLid, timestamp = tstp, usage = `energy(kWh/hh)`) %>%
      filter(meter %in% c("MAC004929", "MAC000010", "MAC004391"),
             year(timestamp) == 2013)
  )

  # remember the commit id so that later we can come back to this point in history
  go_back <- user_space$simulate(
    input %<>%
      mutate(timestamp = floor_date(timestamp, 'hours')) %>%
      group_by(meter, timestamp) %>%
      summarise(usage = sum(usage))
  )

  # dplyr adds attributes to objects when filter is called
  # it's probably some kind of smart pre-computed cache but
  # it messes up object tracking
  #
  # if filter is not a separate step, use subset() instead of
  # filter() to maintain the same object id between commits
  user_space$simulate(
    input %<>% filter(meter == "MAC004929")
  )

  user_space$simulate(
    with(input, plot(timestamp, usage, type = 'p', pch = '.'))
  )

  user_space$simulate(
    x <-
      input %>%
      mutate(hour = hour(timestamp),
             dow  = wday(timestamp, label = TRUE)) %>%
      mutate_at(vars(hour, dow), funs(as.factor)) %>%
      group_by(hour, dow) %>%
      summarise(usage = mean(usage, na.rm = TRUE))
  )

  user_space$simulate(
    with(x, plot(hour, usage))
  )

  user_space$simulate(
    ggplot(x) + geom_point(aes(x = hour, y = usage)) + facet_wrap(~dow)
  )

  user_space$simulate(
    x <-
      input %>%
      mutate(hour = hour(timestamp),
             dow  = wday(timestamp)) %>%
      mutate_at(vars(hour, dow), funs(as.factor))
  )

  user_space$simulate(
    ggplot(x) + geom_boxplot(aes(x = hour, y = usage)) + facet_wrap(~dow)
  )

  user_space$simulate(
    m <- lm(usage ~ hour:dow, x)
  )

  message('Restoring commit ', go_back)
  restore_commit(internal_state, go_back, user_space$session)

  # now try a different house
  user_space$simulate(
    input %<>% filter(meter == "MAC000010")
  )

  user_space$simulate(
    x <-
      input %>%
      mutate(hour = hour(timestamp),
             dow  = wday(timestamp)) %>%
      mutate_at(vars(hour, dow), funs(as.factor))
  )

  user_space$simulate(
    ggplot(x) + geom_boxplot(aes(x = hour, y = usage)) + facet_wrap(~dow)
  )

  # go back again, and try the third house
  message('Restoring commit ', go_back)
  restore_commit(internal_state, go_back, user_space$session)

  user_space$simulate(
    input %<>% filter(meter == "MAC004391")
  )

  user_space$simulate(
    x <-
      input %>%
      mutate(hour = hour(timestamp),
             dow  = wday(timestamp)) %>%
      mutate_at(vars(hour, dow), funs(as.factor))
  )

  user_space$simulate(
    ggplot(x) + geom_boxplot(aes(x = hour, y = usage)) + facet_wrap(~dow)
  )

  invisible(user_space)
}


#' @importFrom datasets iris
simulate_modelling <- function ()
{
  user_space <- eval_space()

  user_space$simulate(x <- lm(Sepal.Width ~ Sepal.Length, iris))
  user_space$simulate(iris2 <- iris)
  user_space$simulate(iris2$Sepal.Length <- iris2$Sepal.Length ** 2)
  user_space$simulate(y <- lm(Sepal.Width ~ Sepal.Length, iris2))
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


#' @export
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
