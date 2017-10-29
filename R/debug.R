stash <- function () internal_state$stash

show_commits <- function ()
{
  lazy_tags <- lazyeval::as.lazy_dots(list(quote(class == 'commit')))
  ids <- storage::os_find(internal_state$stash, lazy_tags)
  lapply(ids, function (id) {
    co <- commit_restore(id, internal_state$stash)
    print(co, simple = TRUE)
  })
  
  invisible()
}


simulate_interactions <- function ()
{
  user_space <- new.env()
  user_command <- function (...) {
    expr <- substitute(...)
    eval(expr, user_space)
    update_current_commit(user_space, expr)
  }

  user_command(x <- 2)
  user_command(y <- x * x)
  user_command(print(y))
  user_command(y <- 100)
}

