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


simulate_user_command <- function (expr, env) {
  expr <- substitute(expr)
  eval(expr, env)

  plot <- tryCatch(recordPlot(), error = function(e)'error')
  if (identical(plot, 'error')) plot <- NULL

  update_current_commit(internal_state, env, plot, expr)
}



simulate_simple_interactions <- function ()
{
  user_space <- new.env()

  simulate_user_command(x <- 2,     user_space)
  simulate_user_command(y <- x * x, user_space)
  simulate_user_command(print(y),   user_space)
  simulate_user_command(y <- 100,   user_space)
}


simulate_modelling <- function ()
{
  user_space <- new.env()

  simulate_user_command(x <- lm(Sepal.Width ~ Sepal.Length, iris), user_space)
  simulate_user_command(iris2 <- iris, user_space)
  simulate_user_command(iris2$Sepal.Length <- iris2$Sepal.Length ** 2, user_space)
  simulate_user_command(y <- lm(Sepal.Width ~ Sepal.Length, iris2), user_space)
}


simulate_london_meters <- function ()
{
  library(dplyr)
  library(lubridate)

  user_space <- new.env()
  try(dev.off())

  simulate_user_command(
    input <-
      system.file("extdata/block_62.csv", package = "experiment") %>%
      readr::read_csv(na = 'Null') %>%
      rename(meter = LCLid, timestamp = tstp, usage = `energy(kWh/hh)`) %>%
      filter(meter %in% c("MAC004929", "MAC000010", "MAC004391"),
             year(timestamp) == 2013),
    user_space
  )

  # dplyr adds attributes to objects when filter is called
  # it's probably some kind of smart pre-computed cache but
  # it messes up object tracking
  simulate_user_command(
    input %<>%
      mutate(timestamp = floor_date(timestamp, 'hours')) %>%
      group_by(meter, timestamp) %>%
      summarise(usage = sum(usage)),
    user_space
  )

  # use subset() instead of filter() to maintain object id
  simulate_user_command(
    with(subset(input, meter == "MAC004929"),
         plot(timestamp, usage, type = 'p', pch = '.')),
    user_space
  )
}
