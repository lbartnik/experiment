
#' Computes hash for any object.
#' 
#' @importFrom digest digest
hash <- function (obj) {
  digest(obj, algo='md5')
}
