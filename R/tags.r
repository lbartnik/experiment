
auto_tags <- function (obj, ...)
{
  c(list(...), extract_tags(obj))
}

extract_tags <- function (obj) UseMethod("extract_tags", obj)

extract_tags.default <- function (obj) list()

extract_tags.lm <- function (obj) list(
  formula = paste0(as.character(obj$call))
)

