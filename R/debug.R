stash <- function () internal_state$stash

show_commits <- function (simple = TRUE)
{
  lazy_tags <- lazyeval::as.lazy_dots(list(quote(class == 'commit')))
  ids <- storage::os_find(internal_state$stash, lazy_tags)
  lapply(ids, function (id) {
    co <- commit_restore(id, internal_state$stash)
    print(co, simple = simple)
  })

  invisible()
}


eval_space <- function ()
{
  e <- new.env()
  e$simulate <- simulate_user_command
  environment(e$simulate) <- e

  e$state <- new.env(parent = globalenv())

  structure(e, class = 'eval_space')
}


simulate_user_command <- function (expr)
{
  env <- parent.env(environment())
  stopifnot(inherits(env, 'eval_space'))

  expr <- substitute(expr)
  message("Evaluating: ", deparse(expr)[[1]], "...")

  # print is necessary for graphics, but we don't want to see the
  # output on the console, thus - print and capture at the same time
  eval_expr <- substitute(print(expr), list(expr = expr))
  capture.output(eval(eval_expr, env$state, enclos = baseenv()))

  plot <- tryCatch(recordPlot(), error = function(e)'error')
  if (identical(plot, 'error')) plot <- NULL

  update_current_commit(internal_state, env$state, plot, expr)
}



simulate_modelling <- function ()
{
  user_space <- eval_space()

  user_space$simulate(x <- lm(Sepal.Width ~ Sepal.Length, iris))
  user_space$simulate(iris2 <- iris)
  user_space$simulate(iris2$Sepal.Length <- iris2$Sepal.Length ** 2)
  user_space$simulate(y <- lm(Sepal.Width ~ Sepal.Length, iris2))
}


simulate_london_meters <- function ()
{
  library(dplyr, quietly = TRUE)
  library(lubridate, quietly = TRUE)
  library(magrittr, quietly = TRUE)
  library(ggplot2, quietly = TRUE)

  user_space <- eval_space()
  try(dev.off(), silent = TRUE)

  user_space$simulate(
    input <-
      system.file("extdata/block_62.csv", package = "experiment") %>%
      readr::read_csv(na = 'Null') %>%
      rename(meter = LCLid, timestamp = tstp, usage = `energy(kWh/hh)`) %>%
      filter(meter %in% c("MAC004929", "MAC000010", "MAC004391"),
             year(timestamp) == 2013)
  )

  user_space$simulate(
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
             dow  = wday(timestamp)) %>%
      mutate_at(vars(hour, dow), funs(as.factor))
  )

  user_space$simulate(
    m <- lm(usage ~ hour:dow, x)
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
    ggplot(x) + geom_boxplot(aes(x = hour, y = usage)) + facet_wrap(~dow)
  )
}
