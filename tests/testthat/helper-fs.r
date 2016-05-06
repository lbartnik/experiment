helper_new_storage <- function ()
{
  path <- file.path(tempdir(), 'storage')
  env  <- parent.frame()
  expr <- bquote(unlink(.(path), recursive = TRUE, force = TRUE))
  do.call(on.exit, list(expr = expr, add = TRUE),  envir = env)
  storage(path, .create = T)
}
