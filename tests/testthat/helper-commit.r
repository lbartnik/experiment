create_sample_env <- function ()
{
  e <- as.environment(list(a = 1, b = list(x = 2)))
  attr(e$a, hash_attribute_name) <- 'hs12'
  attr(e$b, hash_attribute_name) <- 'ab34'
  e
}