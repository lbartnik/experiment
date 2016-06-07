create_sample_env <- function ()
{
  as.environment(list(a = 1, b = list(x = 2)))
}

create_sample_assignment <- function ()
{
  bquote({a <- 1; b <- 2})
}