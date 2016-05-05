
auto_tags <- function (obj, env, ...)
{
  c(list(...), list(class = class(obj)), extract_tags(obj, env))
}

extract_tags <- function (obj, env) UseMethod("extract_tags", obj)

extract_tags.default <- function (obj, env) list()

extract_tags.lm <- function (obj, env)
{
  cl <- match.call(lm, obj$call, envir = env)
  list(
    formula = format(cl$formula),
    data    = as.character(cl$data)
  )
}

