#' Experimental readline-based search interface.
#'
#' @export
try_readline <- function ()
{
  .Call("C_try_readline")
}
