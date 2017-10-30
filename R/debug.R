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
  update_current_commit(env, expr)
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


